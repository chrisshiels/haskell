-- 'id.hs'.


import Data.Function
import Data.List
import System.Environment
import System.Exit
import System.Posix.Types
import System.Posix.User
import Text.Printf


filtergroups :: UserEntry -> [ GroupEntry ] -> [ GroupEntry ]
filtergroups userEntry groupEntries =
  filter (\e -> userName userEntry `elem` groupMembers e) groupEntries


filtergroup :: UserEntry -> [ GroupEntry ] -> [ GroupEntry ]
filtergroup userEntry groupEntries =
  filter (\e -> userGroupID userEntry /= groupID e) groupEntries


sortgroups :: [ GroupEntry ] -> [ GroupEntry ]
sortgroups groupEntries =
  sortBy (\x y -> compare (groupID x) (groupID y)) groupEntries


formatgroups :: [ GroupEntry ] -> [ String ]
formatgroups groupEntries =
  map (\e -> printf "%s(%s)" (show (groupID e)) (groupName e)) groupEntries


intercalategroups :: [ String ] -> String
intercalategroups groups =
  intercalate "," ("":groups)


output :: UserEntry -> GroupEntry -> String -> IO ()
output userEntry groupEntry groups =
  printf "uid=%s(%s) gid=%s(%s) groups=%s(%s)%s\n"
    (show (userID userEntry))
    (userName userEntry)
    (show (groupID groupEntry))
    (groupName groupEntry)
    (show (groupID groupEntry))
    (groupName groupEntry)
    groups


iduid :: UserID -> IO ()
iduid uid =
  do userEntry <- getUserEntryForID uid
     groupEntry <- getGroupEntryForID (userGroupID userEntry)
     groupEntries <- getAllGroupEntries
     filtergroups userEntry groupEntries &
       filtergroup userEntry &
       sortgroups &
       formatgroups &
       intercalategroups &
       output userEntry groupEntry


idusername :: String -> IO ()
idusername username =
  do userEntry <- getUserEntryForName username
     iduid (userID userEntry)


app :: [ String ] -> IO ExitCode
app []   = getEffectiveUserID >>= iduid >>
           return ExitSuccess
app args = mapM_ idusername args >>
           return ExitSuccess


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
