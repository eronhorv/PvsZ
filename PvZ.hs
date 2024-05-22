module PlantsVsZombies where
import Data.List

type Coordinate = (Int, Int)
type Sun = Int

data Plant = Peashooter Int|Sunflower Int|Walnut Int|CherryBomb Int deriving (Show,Eq)

data Zombie = Basic Int Int|Conehead Int Int|Buckethead Int Int|Vaulting Int Int deriving (Show,Eq)

data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)] deriving (Show,Eq)

defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2


class GameElement a where
    getHp :: a -> Int
    setHp :: a -> Int -> a
instance GameElement Zombie where
    getHp x = case x of
        Basic y _ ->y
        Conehead y _ ->y
        Buckethead y _ ->y
        Vaulting y _ ->y
    setHp x n = case x of
        Basic y l -> Basic n l
        Conehead y l ->Conehead n l
        Buckethead y l ->Buckethead n l
        Vaulting y l -> Vaulting n l

instance GameElement Plant where
    getHp x = case x of    
        Peashooter y -> y
        Sunflower y -> y
        Walnut y -> y
        CherryBomb y -> y
    setHp x n = case x of
        Peashooter y -> Peashooter n
        Sunflower y -> Sunflower n
        Walnut y -> Walnut n
        CherryBomb y -> CherryBomb n

setStep :: (Coordinate,Zombie) -> Int -> (Coordinate,Zombie)
setStep (c,z) n = case z of
    Basic y l -> (c,Basic y n)
    Conehead y l -> (c,Conehead y n)
    Buckethead y l ->(c,Buckethead y n)
    Vaulting y l -> (c,Vaulting y n)

zombieStep :: (Coordinate,Zombie) -> (Coordinate,Zombie)              
zombieStep (c,z) = case z of
    Basic x y -> ((fst c,(snd c)-y),Basic x y) 
    Conehead x y -> ((fst c,(snd c)-y),Conehead x y)
    Buckethead x y -> ((fst c,(snd c)-y),Buckethead x y) 
    Vaulting x y -> ((fst c,(snd c)-y),Vaulting x y) 


getPrice :: Plant -> Int
getPrice x = case x of
    Peashooter _ ->  100
    Sunflower _ -> 50
    Walnut _ -> 50
    CherryBomb _ -> 150   



tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
tryPurchase (GameModel s ps zs) c p
    |(fst c>4)||(fst c<0)||(snd c>11)||(snd c<0)=Nothing
    |lookup c ps/=Nothing=Nothing
    |pr>s=Nothing
    |True = Just (GameModel (s-pr) ((c,p):(ps)) zs) where
        pr = getPrice p

placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel s ps zs) z l
    |l>4 ||l<0=Nothing
    |lookup (l,11) zs/=Nothing=Nothing
    |True=Just (GameModel s ps (((l,11),z):(zs)))


performZombieActions :: GameModel -> Maybe GameModel
performZombieActions (GameModel s ps zs)
    |isThereWinnerNormalZombie || isThereWinnerVaulting =Nothing 
    |True=Just (GameModel s newps newzs) where
        newps=[(\(c,pt) ->(c,(setHp pt (getHp pt -frequency c)))) x|x<-ps ] where
            frequency c = length $ filter (==c) (map (fst) $ filter ( \x-> not$ isVaulting $ snd x) zs)
        newzs = allZombieStep zs (map fst ps) where
            allZombieStep [] ps = []
            allZombieStep ((c,z):zs) ps = case isVaulting z of
                True  | elem (fst c,(snd c)-1) ps -> (setStep (zombieStep (c,z)) 1):(allZombieStep zs ps)
                      | elem c ps -> (zombieStep $ setStep (c,z) 1):(allZombieStep zs ps)
                False | elem c ps -> (c,z):(allZombieStep zs ps) 
                _    -> (zombieStep (c,z)):(allZombieStep zs ps) 
        isThereWinnerNormalZombie = elem [] [filter (\(x,y) ->x==z1 && y <= z2) (map fst ps)   | (z1,z2)<-  map fst $ filter  (\x -> (snd $fst $ zombieStep x)<0) zs ]
        isThereWinnerVaulting = elem False [elem (l,1) $map fst ps | ((l,1),Vaulting _ 2) <- zs] --(a gyors vaulting már (l,1) után beér, kivéve ha van (l,1)-en egy növény)
        isVaulting (Vaulting _ 2) =True
        isVaulting _ = False


cleanBoard :: GameModel -> GameModel
cleanBoard (GameModel s ps zs) = (GameModel s ( filter (\x -> getHp (snd x) > 0 ) ps ) ( filter (\x -> getHp (snd x) > 0 ) zs))


performPlantActions :: GameModel -> GameModel
performPlantActions g@(GameModel x y z) = case performActions g of GameModel s ps zs -> GameModel s (killCherrybombs y) zs
    where
    performActions g@(GameModel _ [] _) = g
    performActions (GameModel s p@((pc,pt):ps) zs) = case pt of --a performActions nem hagyja meg a növényeket
        Peashooter _ -> performActions (GameModel s ps (performPeashooterActions pc zs zs)) where
            performPeashooterActions c [] s = []
            performPeashooterActions c l@((zc,zt):zs) s
                |(exists!!(fst c))  && (zc==(fst c, first!!(fst c))) = (zc,damageZombie zt):(performPeashooterActions c zs s)
                |True = (zc,zt):(performPeashooterActions c zs s)
                where
                    first  = [minimum $map snd $filter (\(x,y) ->x==c) $map fst s|c<-[0..4]] --a sorokban a legközelebbi zombi helye
                    exists = [(filter (\(x,y) ->x==c) $ map fst s)/=[]|c<-[0..4]] --van-e a sorokban zombi (keressen-e minimumot)
                    damageZombie x = setHp x (getHp x -1)
        CherryBomb _ -> performActions (GameModel s ps (performCherrybombActions pc zs)) where
            performCherrybombActions c [] = []
            performCherrybombActions c ((zc,zt):zs)
                |inRange c zc=(zc,killZombie zt):(performCherrybombActions c zs)
                |True =(zc,zt):(performCherrybombActions c zs)
                where
                killZombie x = setHp x 0
                inRange cc zc = (abs(fst cc - fst zc) <=1) && (abs (snd cc - snd zc) <=1)
        Sunflower _ -> performActions (GameModel (s+25) ps zs)
        Walnut _ -> performActions (GameModel s ps zs)
    killCherrybombs ps = map (\x@(c,p) -> case p of CherryBomb _ -> (c,setHp p 0);_ -> x) ps


defendsAgainst :: GameModel -> [[(Int, Zombie)]] -> Bool
defendsAgainst g@(GameModel s ps zs) n@(nz:nzs) = defendsAgainstI id g n

defendsAgainstI :: (GameModel -> GameModel) -> GameModel -> [[(Int, Zombie)]] -> Bool
defendsAgainstI _ (GameModel _ _ []) [] = True
defendsAgainstI f g@(GameModel s ps zs) [] = case performZombieActions$cleanBoard$performPlantActions (f g) of
    Nothing -> False
    Just ng -> defendsAgainstI f (addSun$cleanBoard ng) []
defendsAgainstI f g@(GameModel s ps zs) (nz:nzs) = case performZombieActions$cleanBoard$performPlantActions (f g) of 
    Nothing -> False
    Just ng -> defendsAgainstI f (addSun$cleanBoard (placeTheseZombiesInLane ng nz)) nzs

placeTheseZombiesInLane :: GameModel -> [(Int, Zombie)] -> GameModel
placeTheseZombiesInLane g [] = g
placeTheseZombiesInLane g ((l,zt):zs) = case placeZombieInLane g zt l of
    Nothing -> error "Incorrect zombie placement"
    Just ng -> placeTheseZombiesInLane ng zs

addSun :: GameModel -> GameModel
addSun (GameModel s ps zs) = (GameModel (s+25) ps zs)