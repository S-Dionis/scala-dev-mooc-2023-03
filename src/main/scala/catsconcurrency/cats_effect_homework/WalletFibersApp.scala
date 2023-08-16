package catsconcurrency.cats_effect_homework

import cats.Parallel
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import cats.syntax.all._

import scala.concurrent.duration.DurationInt

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def run: IO[Unit] = for {
      _       <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
      fib1 <- (wallet1.topup(100) >> IO.sleep(100.millisecond)).foreverM.start
      fib2 <- (wallet2.topup(100) >> IO.sleep(500.millisecond)).foreverM.start
      fib3 <- (wallet3.topup(100) >> IO.sleep(2000.millisecond)).foreverM.start
      print <- {
        wallet1.balance.flatMap(balance => IO.print(s"Wallet1 balance: $balance ")) >>
          wallet2.balance.flatMap(balance => IO.print(s"Wallet2 balance: $balance ")) >>
          wallet3.balance.flatMap(balance => IO.println(s"Wallet3 balance: $balance")) >>
          IO.sleep(1000.millisecond)
      }.foreverM.start
      _ <- IO.readLine
      _ <- fib1.cancel >> fib2.cancel >> fib3.cancel >> print.cancel
    } yield ()


}
