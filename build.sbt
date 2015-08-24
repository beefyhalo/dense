libraryDependencies += "com.chuusai" %% "shapeless" % "2.2.4"

initialCommands in console := "import shapeless.test._, dense._, Dense._, ops._, syntax._"

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-Xfatal-warnings"
//  "-language:existentials"
)

// scalacOptions in Test += "-Xlog-implicits"
