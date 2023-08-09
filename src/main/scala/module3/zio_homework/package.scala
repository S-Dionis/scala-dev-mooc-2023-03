package module3

import module3.zioConcurrency.printEffectRunningTime
import zio.{Has, Task, ULayer, URIO, ZIO, ZLayer, clock}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.{Source, StdIn}
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */



  lazy val guessProgram = for {
    console <- ZIO.environment[Console].map(_.get)
    random <- ZIO.environment[Random].map(_.get)
    guess <- random.nextIntBounded(2)
    _ <- console.putStr("Enter the number between 1 and 3\n")
    line <- console.getStrLn
    i <- ZIO.effect(line.toInt).orElseFail(new Throwable("Not a number\n"))
    uGuess <- if (i > 3 || i < 1) ZIO.fail(new Throwable("Should be between 1 and 3\n")) else ZIO.succeed(i)
    _ <- console.putStr(if (uGuess == guess + 1) "you won" else "you loose" + s" I guessed ${guess  + 1}\n")
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile: Task[Boolean] => Task[Unit] = (effect: Task[Boolean]) => for {
    value <- effect
    _ <- if (value) ZIO.succeed(()) else doWhile(effect)
  } yield ()

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault = for {
    c <- config.load.foldM(
      _ => ZIO.succeed(config.AppConfig("mur", "meow")),
      success => ZIO.succeed(success)
    )
    _ <- ZIO.effect(println(c))
  } yield c


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff = for {
    random <- ZIO.environment[Random].map(_.get)
    _ <- ZIO.sleep(1 seconds)
    number <- random.nextIntBounded(11)
  } yield number

  /**
   * 4.2 Создайте коллекцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = Array.fill(10)(eff).toList

  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = printEffectRunningTime(for {
    int <- effects.reduceLeft((a, b) => a.zipWith(b) (_ + _))
    _ <- ZIO.accessM[Console](c => c.get.putStrLn(int.toString))
  } yield (int))


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp =
    printEffectRunningTime(for {
      it <- ZIO.foreachParN(10)(effects)(identity)
      int <- ZIO.effect(it.sum)
      _ <- ZIO.accessM[Console](c => c.get.putStrLn(int.toString))
    } yield (int))






  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  type RunningTimeService = Has[RunningTimeService.Service]

  object RunningTimeService {
    trait Service {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock, E, A]
    }

    val live = ZLayer.succeed(new Service {
      override def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock, E, A] = for {
        start <- clock.currentTime(TimeUnit.SECONDS)
        r <- zio
        end <- clock.currentTime(TimeUnit.SECONDS)
        _ <- ZIO.effect(println(s"Running time ${end - start}")).orDie
      } yield r
    })

    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with RunningTimeService with Clock, E, A] = for {
      service <- ZIO.environment[RunningTimeService].map(_.get)
      it <- service.printEffectRunningTime(zio)
    } yield it



  }
  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg: ZIO[RunningTimeService with Console with Clock with Random, Throwable, Unit] = for {
    runningTimeService <- ZIO.environment[RunningTimeService].map(_.get)
    _ <- runningTimeService.printEffectRunningTime(app)
  } yield ()

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  val env: ZLayer[Any, Nothing, RunningTimeService] = RunningTimeService.live

  lazy val runApp: ZIO[zio.ZEnv, Throwable, Unit] = appWithTimeLogg.provideCustomLayer(env)


}
