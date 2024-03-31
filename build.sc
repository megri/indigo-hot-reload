import mill._, scalalib._, scalajslib._

import scala.collection.mutable


object ElectronModule {
  class Worker(val dstBase: os.Path) extends AutoCloseable {
    private val updateTimes = mutable.Map[os.Path, Long]()
    private var process: os.SubProcess = null

    def updateApp(electronEntrypoint: os.Path, watchedSourceBases: Seq[os.Path])(implicit ctx: mill.api.Ctx): Unit = {
      val updates = (
        for {
          srcBase <- geny.Generator(watchedSourceBases: _*)
          (srcPath, srcAttrs) <- os.walk.stream.attrs(srcBase)
          wasUpdated = copyIfUpdated(srcBase, srcPath, srcAttrs)
          if wasUpdated
        } yield srcPath
      ).toSeq

      if (!processIsDead() && updates.exists(_ == electronEntrypoint)) {
        sendPM("DESTROY")
        process.destroy()
        // process.wrapped.onExit().get()
        // we could await process termination by uncommenting the line above, or we can go ahead and just set process to
        // null which should in theory be faster.
        process = null
      }

      if (processIsDead()) {
        T.log.info("Restarting app.")

        process = os
          .proc("npx", "--no-install", "electron", electronEntrypoint.last)
          .spawn(
            cwd = dstBase,
            stdout = os.ProcessOutput.Readlines(line => T.log.info(s"[from process] $line")),
          )
      } else {
        sendPM("RELOAD")
      }
    }

    def close(): Unit = {
      if (process != null && process.isAlive()) {
        process.stdin.writeLine("DESTROY")
        process.stdin.flush()
        process.destroy()
      }
      process = null
    }

    private def copyIfUpdated(srcBase: os.Path, src: os.Path, srcAttrs: os.StatInfo)(implicit
        ctx: mill.api.Ctx
    ): Boolean = {
      val dst = dstBase / src.subRelativeTo(srcBase)
      if (srcAttrs.isDir) {
        os.makeDir.all(dst)
        false
      } else {
        val srcUpdatedAt = srcAttrs.mtime.toMillis
        val dstUpdatedAt = updateTimes.getOrElse(dst, 0L)
        val shouldUpdate = dstUpdatedAt < srcUpdatedAt
        if (shouldUpdate) {
          def relDst = dst.subRelativeTo(dstBase)
          T.log.debug(s"Replacing '$relDst'.")
          if (srcAttrs.isSymLink) {
            os.remove(dst)
            os.symlink(dst, dstBase / os.readLink.absolute(src).subRelativeTo(srcBase))
          } else {
            os.copy(src, dst, replaceExisting = true, copyAttributes = true)
          }
          updateTimes(dst) = srcUpdatedAt
        }
        shouldUpdate
      }
    }

    private def processIsDead() = process == null || !process.isAlive()

    private def sendPM(msg: String)(implicit ctx: mill.api.Ctx): Unit = {
      if (!processIsDead()) {
        process.stdin.writeLine(msg)
        process.stdin.flush()
      } else {
        T.log.info(s"Wanting to send '$msg' to the process but it is dead.")
      }
    }
  }
}


trait ElectronModule extends ScalaJSModule {
  def electronVersion: T[Option[String]] = None

  def appRoot = T.source { PathRef(millSourcePath / "app-root") }

  def devWorker = T.worker {
    new ElectronModule.Worker(T.dest)
  }

  def installElectron = T {
    val electronArtifact = electronVersion() match {
      case Some(v) => s"electron@$v"
      case None    => "electron"
    }
    T.log.debug(s"Installing $electronArtifact…")
    os.proc("npm", "install", "-D", electronArtifact).call(cwd = T.dest)
    T.log.debug(s"Electron installed to '${T.dest}'.")
    PathRef(T.dest)
  }

  def electronEntrypoint: T[os.Path] = appRoot().path / "index.js"

  def dev = T {
    val electronRootPath = installElectron().path
    val appRootPath = appRoot().path
    val appCodePath = fastLinkJS().dest.path
    devWorker().updateApp(electronEntrypoint(), Seq(electronRootPath, appRootPath, appCodePath))
  }
}


object game extends ElectronModule {
  def scalaVersion = "3.3.3"

  def ammoniteVersion = "3.0.0-M1"

  def scalacOptions = Seq("-Wunused:imports")

  def scalaJSVersion = "1.16.0"

  val indigoVersion = "0.16.0"

  def ivyDeps = Agg(
    ivy"io.indigoengine::indigo::$indigoVersion",
    ivy"io.indigoengine::indigo-json-circe::$indigoVersion",
    ivy"io.indigoengine::indigo-extras::$indigoVersion",
    ivy"com.outr::scribe::3.13.0",
  )

  object test extends ScalaJSTests with TestModule.Munit {
    def ivyDeps = Agg(
      ivy"org.scalameta::munit::1.0.0-M10",
    )
  }
}