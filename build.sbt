name := "hackathon"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.amazon.alexa" % "alexa-skills-kit" % "1.3.0",
  "com.github.fommil" %% "spray-json-shapeless" % "1.3.0"
)

enablePlugins(AwsLambdaPlugin)
