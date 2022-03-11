package webapp

import zio._
import zhttp.http._
import zhttp.service.Server

/**
 * REPOSITORY
 *
 * https://github.com/jdegoes/zio-webapp
 *
 * Download and compile the project using SBT or IDE of choice.
 */

object what_is_zio {
  /*
   * `Runnable` provides a simple way to describe a task that can be run.
   */
  object phase1 {
    trait Runnable { self =>
      def run(): Unit

      // Going from the world of statements to the world of values!

      /*
      * Concatenation method for merging Runnables
      */
      def ++ (that: Runnable): Runnable = () => {
        self.run()
        that.run()
      }

      // We could also add a version to run two runnables in parallel

      /*
      * We try the one on the left side. But if it fails, we try the one on the right side
       */
      def orElse(that: Runnable): Runnable = () => {
        try {
          self.run()
        } catch {
          case _ : Throwable => that.run()
        }
      }

      /*
       * EXERCISE
       *
       * Add a `retry` method that retries failed runnables for the specified
       * number of times.
       */
      def retry(n: Int): Runnable = () => {
        try {
          self.run()
        } catch {
          case t : Throwable =>
            if (n > 1) retry(n - 1).run()
            else throw t
        }
      }
    }

    val printAndRead: Runnable = () => {
      println("What is your name?")
      val name = scala.io.StdIn.readLine()
      println(s"Hello, $name!")
    }

    val retried10 = printAndRead.retry(10)

    def uploadFile(url: String): Runnable = ???

    // Example of how expressive and powerful this is
    uploadFile("foo,.txt").retry(100).orElse(() => ???)

    /*
     * EXERCISE
     *
     * Implement a resilient upload.
     */
    lazy val resilientUpload = uploadFile("foo.txt").retry(100)

    // run() works at a sort of interpreter or executor
    resilientUpload.run()
  }

  // The runnable is limited because it returns Unit...
  // Which lead us to this "phase2"

  object phase2 {
    import scala.util.Try

    type Runnable = Callable[Unit] // Everything we did before with Runnable we can now do with Callable

    /*
     * `Callable[A]` provides a simple way to describe a task that can be run to
     * produce a success value of a certain type `A`.
     *
     * This [+A] is a success channel for this type
     */
    trait Callable[+A] { self =>
      def call(): A

      // Sequential Composition
      // In Scala this is called flatMap
      // You can also see the structure of a callback here
      def andThen[B](f: A => Callable[B]): Callable[B] =
        new Callable[B] {
          def call(): B = f(self.call()).call()
        }

      /*
       * EXERCISE
       *
       * Implement a sequential operator that computes two callables in
       * sequence.
       */
      def sequential[B](that: Callable[B]): Callable[(A, B)] = () => {
        (self.call(), that.call()) // This operator is like Zip
      }

      // With ZIO we separate the How from the What
      // We can change how something is executed without having to refactor our code

      def parallel[B](that: Callable[B]): Callable[(A, B)] =
        new Callable[(A, B)] {
          def call(): (A, B) = {
            var a: Option[Either[Throwable, A]] = None
            var b: Option[Either[Throwable, B]] = None

            val countDownLatch = new java.util.concurrent.CountDownLatch(2)

            new Thread {
              override def run() = {
                a = Some(Try(self.call()).toEither)

                countDownLatch.countDown()
              }
            }.start

            new Thread {
              override def run() = {
                b = Some(Try(that.call()).toEither)

                countDownLatch.countDown()
              }
            }.start()

            countDownLatch.await()

            val a0 = a.get.fold(throw _, identity(_))
            val b0 = b.get.fold(throw _, identity(_))

            (a0, b0)
          }
        }

      def retry(n: Int): Callable[A] =
        () => {
          try {
            self.call()
          } catch {
            case t: Throwable =>
              if (n > 1) retry(n - 1).call() else throw t
          }
        }
    }

    def downloadFile(url: String): Callable[String] =
      () => {
        val source = scala.io.Source.fromURL(url)

        try source.mkString("\n")
        finally source.close()
      }

    /*
     * EXERCISE
     *
     * Describe downloading two files in parallel, where if either download
     * fails, it is repeatedly tried until it succeeds.
     */
    lazy val downloadFilesInParallel =
      downloadFile("foo.txt").retry(Int.MaxValue).parallel(
        downloadFile("bar.txt").retry(Int.MaxValue)
      )

    // If either one fails, we need to download them both! This is solving a different problem
    lazy val downloadFilesInParallel2 =
      downloadFile("foo.txt").parallel(
        downloadFile("bar.txt")
      ).retry(Int.MaxValue)
  }

  // How does a Callable fail? It throws
  // Using Callable doesn't allow us to dynamically type our errors

  // Callable gives us statically typed success values
  // But like Runnable, no statically typed errors

  /*
   * `IO[E, A]` provides a simple way to describe a task that can be run to
   * produce either a failure value of type `E`, or a success value of type `A`.
   */
  object phase3 {
    import scala.util.Try

    // Here, Callable could be defined as:
    type Callable[+A] = IO[Throwable, A]

    // But now we have the ability to define Errors not only as Throwables

    // There are only 3 ways to fail
    // Business/Domain Errors -> Sealed trait with case classes

    // The Scala compiler can make sure that when we try to recover from that error we must handle all the 3 possibilities

    // We can also define an Infallible computation, which never fails, like this:

    // def run(): Either[Nothing, A]
    type Infallible[+A] = IO[Nothing, A]

    type Unending[+E] = IO[E, Nothing]

    // Continuing over "Infallible" and IO[Nothing, A]:

    // Everything extends "Any"
    // "Nothing" is at the bottom of everything
    // Nothing is a special subtype of every other type
    // This is useful also for Types you cannot construct values for
    // For example:
    final abstract class Void {
      def absurd[A]: A
    }
    // There are no values and there can be no values of type "Void"
    // Why?
    // First, there would need to be a constructor or we'd need to have a subtype of Void, but here we can't
    // Because it's final abstract and there is no other type related to it
    // There are no problems talking about types that have no set of associated values. This may be a bit mind-bending.

    //Even if we would:
    trait Void2 {
      def absurd[A]: A
    }
    // or class Void2
    //
    // We can't implement a subtype of this. Because we can't implement absurd

    // For any value of type A, you can return a value of type A
    def testVoid(void: Void2): Int = void.absurd[Int]

    // There is no posssible way anyone could create a function which would

    // 1. You can return null and cast it to A (but of course that will blow up and null pointer)
    // 2. You can throw an exception
    // 3. or you can never return
    // These types are called uninhabitable

    // In a similar fashion, there are no values of type Nothing.
    // Also, there can be no values of type Nothing, because Nothing is a subtype of every other type
    //
    // Nothing <: String with Int with ...
    //
    // (with every other possible type)

    def testNothing(nothing: Nothing): Int = nothing

    // "Nothing" can be treated has anything, because it extends every other type


    // Is it left of Nothing, or right of A?
    // Let's see with the help of the compiler, by implementing this function
    def unifyRight[A](either: Either[Nothing, A]): A =
      either match {
        case Left(nothing) => nothing
        case Right(a) => a
      }
    // This is a proof that we can always get an A from an Either[Nothing, A]
    // (also, again, because Nothing is a subtype of all types)


    // The previous form (Callable) wasn't symmetrical, but this one is
    // If it fails it gives an E, if it succeeds it gives an A
    trait IO[+E, +A] { self =>
      def run(): Either[E, A]

      /*
       * EXERCISE
       *
       * Implement a `catchAll` method that catches all errors.
       * {{{
       * readLine.catchAll(_ => printLine("There was an error!"))
       * }}}
       */
      def catchAll[E2, A2 >: A](f: E => IO[E2, A2]): IO[E2, A2] =
        new IO[E2, A2] {
          def run(): Either[E2, A2] = self.run().left.flatMap(e => f(e).run())
        }

      def either: IO[Nothing, Either[E, A]] =
        new IO[Nothing, Either[E, A]] {
          def run(): Either[Nothing, Either[E, A]] = Right(self.run())
        }

      def flatMap[EE >: E, B](f: A => IO[EE, B]): IO[EE, B] =
        new IO[EE, B] {
          def run(): Either[EE, B] = self.run().flatMap(a => f(a).run())
        }

      def map[B](f: A => B): IO[E, B] =
        new IO[E, B] {
          def run(): Either[E, B] = self.run().map(f)
        }

      /*
       * EXERCISE
       *
       * Implement a fallback operator.
       */
      def orElse[E2, A2 >: A](that: IO[E2, A2]): IO[E2, A2] =
        self.catchAll(_ => that)
    }
    object IO {
      def attempt[A](a: => A): IO[Throwable, A] =
        new IO[Throwable, A] {
          def run(): Either[Throwable, A] = Try(a).toEither
        }

      def fail[E](e: => E): IO[E, Nothing] =
        new IO[E, Nothing] {
          def run(): Either[E, Nothing] = Left(e)
        }

      def succeed[A](a: => A): IO[Nothing, A] =
        new IO[Nothing, A] {
          def run(): Either[Nothing, A] = Right(a)
        }
    }

    def downloadFile(url: String): IO[Throwable, String] =
      IO.attempt {
        val source = scala.io.Source.fromURL(url)

        try source.mkString("\n")
        finally source.close()
      }
  }

  // With every step we are making one less polymorphic function and are making it more polymorphic
  // We started with Runnable
  // Then we went to Callable
  // Then IO

  // Now we'll want to have polymorphic input types
  // ZIO[-R, +E, +A]
  // - , contravariance, is for feeding it in/input channel

  // ZIO is a fully polymorphic form of Runnable

  object phase4 {
    import scala.util.Try

    // In order for this to be a strict superset of IO, what do we need to prove?
    // The old thing fell out as a specialized case of the new thing

    // def run(): Either[E, A]
    // def run(unit: Unit): Either[E, A]
    type IO[+E, +A] = ZIO[Unit, E, A]

    // Mental model of ZIO:
    // R => Either[E, A]
    //
    // A function which takes an E, and returns either an E or an A

    // What is a ZIO effect? It is basically a function
    // It turns out that almost every part of your program has 3 needs:
    //
    // 1. The need to succeed with some value. This is the most obvious one. (Except special cases like a web server loop)
    // 2. The need to fail. Because things can go wrong, the API can't be reached, some service is down, etc.
    // 3. The context. Information from the surrounding environment. Sometimes a database connection, an API key, a user configuration, etc

    // That's what ZIO is
    // A little piece of your program. Compositional piece. You can zoom into that part of your program and see these things about that part.
    // And you can zoom out of your whole program, and still see these things
    // Your whole application will look like this, macroscopically and microscopically. The only thing that changes is the scale.

    // Inside this R, you will find essentially two kinds of things:
    // 1. Global application dependencies. Like user repos.
    // 2. The other subset is when you need something locally. An example is if you're building a web server and your request handlers will need access to the request
    // another example is, if your program is in an environment with a lot of users, you'll probably need the session information.

    // Global dependencies (very important and useful also for testability of your program), and Local dependencies (domain-specific context)

    trait ZIO[-R, +E, +A] { self =>
      def map[B](f: A => B): ZIO[R, E, B] =
        new ZIO[R, E, B] {
          def run(r: R): Either[E, B] = self.run(r).map(f)
        }

      def flatMap[RR <: R, EE >: E, B](f: A => ZIO[RR, EE, B]): ZIO[RR, EE, B] =
        new ZIO[RR, EE, B] {
          def run(r: RR): Either[EE, B] = self.run(r).flatMap(a => f(a).run(r))
        }

      def run(r: R): Either[E, A]
    }
    object ZIO {
      def attempt[A](a: => A): ZIO[A, Throwable, A] =
        new ZIO[Any, Throwable, A] {
          def run(r: Any): Either[Throwable, A] = Try(a).toEither
        }

      def fail[E](e: => E): ZIO[Any, E, Nothing] =
        new ZIO[Any, E, Nothing] {
          def run(r: Any): Either[E, Nothing] = Left(e)
        }

      def succeed[A](a: => A): ZIO[Any, Nothing, A] =
        new ZIO[Any, Nothing, A] {
          def run(r: Any): Either[Nothing, A] = Right(a)
        }
    }
  }
}

// There are some changes in ZIO 2

/*
 * EXERCISE
 *
 * Implement a number guessing game.
 */
object RandomNumberGuessingGame extends ZIOAppDefault {

  import zio._

  // in ZIO 2, Apps compose
  // "<>" is Append/Compose operator
  val app3 = RandomNumberGuessingGame <> RandomNumberGuessingGame
  // This will combine the layers of these apps
  // The lane, the layer and the hooks

  trait Foo1 {
    def bar: Int
  }

  trait Foo2 {
    def bar: String
  }
  //Foo1 with Foo2
  //
  // What is this?
  // Well, Scala 3 can do this intersection in a way which makes sense
  // You'll find it's not possible to create a type which extends both Foo1 and Foo2, because either both have to return an Int, or a String
  new Foo1 with Foo2 {
    def bar: Int with String = ???
  }
  // This is inhabitable

  // A Phantom time is used to tracking at compile time, but values of that type do not exist

  // The Mental model for ZIO 1 is:
  // R => Either[E, A]

  // For ZIO 2, it changes and the mental model is:
  // ZEnvironment[R] => Either[E, A]
  //
  // Map, Type Console, and get Implementation of Console
  // Give it Random, and it gives you an implementation of Random
  //
  // This Phantom type parameter is just to keep track of what is in the map. There are never values of that type

  // We don't want this function to fail, so we'll use "Nothing"
  // For this, we will use ".orDie"
  def getGuess: ZIO[Console, Option[Nothing], Int] =
    for { // (for {
      _ <- Console.printLine("I'm thinking of a number between 1 and 10. Try to guess it").orDie
      line <- Console.readLine.orDie
      guess <- ZIO.fromOption(line.toIntOption)
    } yield guess //.provide(ZEnv.live) // This allows to specify that the result no longer needs anything in terms of -R

  type X = ZIO[Console with Random, Nothing, Unit]
  type Y = ZIO[Any, Nothing, Unit]

  // Type Signature differences on ZIO[-R, , ]
  // This requires a ZEnvironment with a Console and Random inside it

  def enterGame(number: Int): ZIO[Console with Random, Nothing, Unit] = {
    (for {
      guess <- getGuess.tapError(_ => Console.printLine("That's not a number!").orDie).eventually // Attach an error handler to this effect, which will be called in the event of an error.
      // eventually will give a new effect which will just repeat this until it succeeds
      _ <- if (guess == number) Console.printLine("You guessed it!").orDie
      else if (guess < number) Console.printLine("Too low!").orDie *> ZIO.fail(())
      else Console.printLine("Too high!").orDie *> ZIO.fail(())
    } yield ()).eventually
    // Please note how we added "orDie" also to avoid another problem which could occur about retrying
  }

  def run =
    for {
      number <- Random.nextIntBetween(1, 11)
      _ <- enterGame(number)
    } yield ()

  // Earlier forms:

  //  def enterGame(number: Int) =
  //    for {
  //      _     <- Console.printLine("I'm thinking of a number between 1 and 10. Try to guess it")
  //      line  <- Console.readLine
  //      // *> Does sequential composition and returns whatever is on the right side
  //      guess <- ZIO.attempt(line.toInt).orElse {
  //        Console.printLine(s"$line is not a number. Try again.") *>
  //          enterGame(number)
  //      }
  //    } yield guess
  //
  //  def run =
  //    for {
  //      number <- Random.nextIntBetween(1, 11)
  //      _      <- enterGame(number)
  //    } yield ()


  //  def run =
  //    //getArgs // To access arguments
  //  for {
  //    number <- Random.nextIntBetween(1, 11) // Upper bound range is exclusive
  //    _      <- Console.printLine("I'm thinking of a number between 1 and 10. Try to guess it")
  //    guess  <- Console.readLine
  //    //_    <- exit(ExitCode.success)
  //  } yield () // The run function is allowed to return Unit. It no longer enforces an exit code.
  //}

}

/*
 * EXERCISE
 *
 * Implement a word count app that counts words in standard input or a file
 * (your choice).
 */
object WordCountApp extends ZIOAppDefault {

  // import zio.stream._

  def countWords(line: String, counter: Ref[Map[String, Int]]) =
    ZIO.foreach(line.split("\\s+")) { word =>
      counter.update { map =>
        map + (word -> (map.getOrElse(word, 0) + 1))
      }
    }

  // Load the contents of a file and then count the words
  // We should use zio.stream._ for this, but let's use a lower level mechanism here for now
  def countWordsInFile(file: String) = {
    for {
      counter <- Ref.make(Map.empty[String, Int])
      source <- ZIO.attemptBlocking(scala.io.Source.fromFile(file))
      _ <- ZIO.foreach(scala.io.Source.fromFile(file).getLines().toVector) { line =>
          countWords(line, counter)
        }.ensuring(ZIO.attempt(source.close()).orDie) //.ensuring(ZIO.succeed(source.close()))
      } yield ()
    }

  def run =
    for {
      args <- getArgs
      file <- ZIO.fromOption(args.headOption).tapError(_ => Console.printLine("You need to specify a file!"))
      _ <- countWordsInFile(file)
    } yield ()
}

object MyWebApp extends ZIOAppDefault {
  val app = Http.collect[Request] {
    case req @ Method.GET -> !! / "text" =>
      Response.text("Hello World!")

    case req @ Method.GET -> !! / "users" =>
      Response.text("Hello World!")
  }

  def run = Server.start(8090, app)
}
