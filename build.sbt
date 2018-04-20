organization := "com.inkenkun.x1"

name := "cats-sandbox"
version := "0.0.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint",               // enable handy linter warnings
  "-Xfatal-warnings",     // turn compiler warnings into errors
  "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.0",
  "org.scalatest" %% "scalatest" % "3.0.1"    % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
