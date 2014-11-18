lazy val root = (project in file(".")).
    settings(
        name := "breezeglm",
        version := "0.1",
        scalaVersion := "2.11.4"
    )

libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % "0.8.1",
    "org.scalanlp" %% "breeze-natives" % "0.8.1"
)

resolvers ++= Seq(
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)
    
