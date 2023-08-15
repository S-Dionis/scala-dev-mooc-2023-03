package module4.homework.services

import zio.Has
import zio.Task
import module4.homework.dao.entity.{Role, RoleCode, User, UserId, UserToRole}
import module4.homework.dao.repository.UserRepository
import module4.homework.dao.repository.UserRepository.UserRepository
import zio.ZIO
import zio.RIO
import zio.ZLayer
import zio.macros.accessible
import module4.phoneBook.db

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx
        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] = userRepo.list()

        def listUsersDTO(): RIO[db.DataSource,List[UserDTO]] = listUsers().flatMap{ users =>
          val roles = users.map{ user =>
            for {
              role <- userRepo.userRoles(user.typedId)
            } yield UserDTO(user, role.toSet)
          }
          ZIO.collectAll(roles)
        }

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = for {
          userDto <- dc.transaction(
            for {
              user <- userRepo.createUser(user)
              role <- userRepo.createRole(Role(roleCode.code, roleCode.code.capitalize))
              _ <- userRepo.insertRoleToUser(role.typedCode, user.typedId)
            } yield UserDTO(user, Set(role)))
        } yield userDto

        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = (for {
          users <- userRepo.listUsersWithRole(roleCode)
        } yield users)
          .map(it => it.map(UserDTO(_, Set(Role(roleCode.code, roleCode.code.capitalize)))))


    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] =
      ZLayer.fromService[UserRepository.Service, UserService.Service](service =>
        new Impl(service))
}

case class UserDTO(user: User, roles: Set[Role])