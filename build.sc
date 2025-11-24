//| mill-version: 1.0.6
package build

import mill.*, scalalib.*, publish.*, scalajslib.*

object graphi extends ScalaModule, ScalaJSModule, SonatypeCentralPublishModule {
	def scalaVersion = "3.7.1"
	def scalaJSVersion = "1.19.0"

	def scalacOptions = super.scalacOptions() ++ Seq(
		"-rewrite",
		"-source", "3.7-migration",
		"-deprecation",
		"-Wvalue-discard",
		"-Xfatal-warnings",
	)

	def publishVersion = "0.0.2"

	def pomSettings = PomSettings(
		description = "A simple, immutable graph library for Scala.",
		organization = "io.github.acloudmovingby",
		url = "https://github.com/acloudmovingby/graphi",
		licenses = Seq(License.MIT),
		versionControl = VersionControl.github("acloudmovingby", "graphi"),
		developers = Seq(Developer("acloudmovingby", "Chris Oates", "https://github.com/acloudmovingby"))
	)

	def sonatypeUri = "https://maven.pkg.github.com/my-org/my-repo"

	def sonatypeSnapshotUri = sonatypeUri

	def sonatypeCreds = for {
		user <- sys.env.get("GITHUB_USER")
		token <- sys.env.get("GITHUB_TOKEN")
	} yield coursier.core.Authentication(user, token)

	def mvnDeps = Seq(
		mvn"org.scala-js::scalajs-dom::2.8.0"
	)

	object test extends ScalaJSTests {
		def mvnDeps = Seq(mvn"com.lihaoyi::utest::0.9.1")
		def testFramework = "utest.runner.Framework"
	}
}
