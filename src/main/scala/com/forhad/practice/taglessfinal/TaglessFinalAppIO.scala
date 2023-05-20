package com.forhad.practice.taglessfinal
import cats._
import cats.effect._
import cats.implicits._
import cats.effect.implicits._

object TaglessFinalAppIO extends IOApp {
  case class User(username: String, age: Int)
  object User {
    implicit val showUser: Show[User] = Show.show(user => s"${user.username} is ${user.age} years old")
  }
  trait UserRepository {
    def addUser(user: User): IO[Unit]
    def getUser: IO[List[User]]
  }

  object UserRepository {
    def impl: IO[UserRepository] = Ref.of[IO, Map[String, User]](Map.empty).map(ref => {
      new UserRepository {
        override def addUser(user: User): IO[Unit] =
          ref.update { usersMap =>
            usersMap + (user.username -> user)
          }

        override def getUser: IO[List[User]] = ref.get.map(usersMap => usersMap.values.toList)
      }
    })
  }

  trait UserService {
    def printUsers(users: List[User]): IO[Unit]
  }
  object UserService {
    def impl: UserService = new UserService {
      override def printUsers(users: List[User]): IO[Unit] =
        users.parTraverse_(user => IO.println(user.show))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      repo <- UserRepository.impl
      service = UserService.impl
      users = List(User("Forhad", 28), User("Forhad+1", 29), User("Forhad+2", 30))
      _ <- users.parTraverse_(repo.addUser)
      savedUsers <- repo.getUser
      _ <- service.printUsers(savedUsers)
    } yield ExitCode.Success
  }
}
