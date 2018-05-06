module Homework8 where
import Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons (e@Emp {empFun = empFun}) (GL es glFun) = GL (e:es) (empFun + glFun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root xs) = f root (map (treeFold f) xs)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = (withBoss, withoutBoss) where
    withoutBoss = mconcat (map (uncurry moreFun) results)
    withBoss = glCons boss (mconcat (map snd results))

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

printGuestList :: GuestList -> String
printGuestList(GL employees fun) =
                    "Fun score : "
                    ++ show fun ++ "\n"
                    ++ unlines (map empName employees)
main :: IO ()
main = do
  contents <- readFile "homework8/company.txt"
  let employeeTree = read contents
      guestList = maxFun employeeTree
      prettyStr = printGuestList guestList
  putStrLn prettyStr