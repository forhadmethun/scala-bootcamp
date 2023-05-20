package com.forhad.practice.taglessfinal

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.effect.std.Console
import cats.implicits._

object TaglessFinalAppTF extends IOApp {
  case class User(username: String, age: Int)
  object User {
    implicit val showUser: Show[User] = Show.show(user => s"${user.username} is ${user.age} years old")
  }
  trait UserRepository[F[_]] {
    def addUser(user: User): F[Unit]
    def getUser: F[List[User]]
  }

  object UserRepository {
    def impl[F[_]: Sync]: F[UserRepository[F]] = Ref.of[F, Map[String, User]](Map.empty).map(ref => {
      new UserRepository[F] {
        override def addUser(user: User): F[Unit] =
          ref.update { usersMap =>
            usersMap + (user.username -> user)
          }

        override def getUser: F[List[User]] = ref.get.map(usersMap => usersMap.values.toList)
      }
    })
  }

  trait UserService[F[_]] {
    def printUsers(users: List[User]): F[Unit]
  }
  object UserService {
    def impl[F[_]: Console: Parallel]: UserService[F] = new UserService[F] {
      override def printUsers(users: List[User]): F[Unit] =
        users.parTraverse_(user => Console[F].println(user.show))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      repo <- UserRepository.impl[IO]
      service = UserService.impl[IO]
      users = List(User("Forhad", 28), User("Forhad+1", 29), User("Forhad+2", 30))
      _ <- users.parTraverse_(repo.addUser)
      savedUsers <- repo.getUser
      _ <- service.printUsers(savedUsers)
    } yield ExitCode.Success
  }
}
