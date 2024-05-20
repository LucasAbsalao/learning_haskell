data Animal = Cisnal | Iguanoide | Narvale | Null
  deriving (Eq, Show)

animalList :: [String] -> [String] -> [Animal]
animalList [] list2 = []
animalList list1 [] = []
animalList (a:as) (b:bs)
    | parentesco a b >= 0.1 && parentesco a b <= 0.3 = [Cisnal] ++ animalList as bs
    | parentesco a b >= 0.4 && parentesco a b <= 0.7 = [Iguanoide] ++ animalList as bs
    | parentesco a b >= 0.1 && parentesco a b <= 0.3 = [Narvale] ++ animalList as bs
    | otherwise = animalList as bs
        where parentesco a b = charEquals a b / fromIntegral (max (length a) (length b))  

charEquals :: String -> String -> Float
charEquals [] str2 = 0
charEquals str1 [] = 0
charEquals (a:as) (b:bs)
    |a==b = 1 + charEquals as bs
    |otherwise = charEquals as bs 

countAnimals :: [Animal] -> [Int]
countAnimals lista = [countEach Cisnal lista]++[countEach Iguanoide lista]++[countEach Narvale lista]

countEach :: Animal -> [Animal] -> Int
countEach animal [] = 0
countEach animal (a:as)
    | animal==a = 1 + countEach animal as
    | otherwise = countEach animal as 

dna2 :: [String] -> [String] -> [Int]
dna2 a b = countAnimals(animalList a b)

main = do
  firstExtract <- words <$> getLine                       -- equivalente a (read firstExtract :: [String])
  secondExtract <- words <$> getLine
  let result = dna2 firstExtract secondExtract
  print result



--["UTHUNOHUNOHU", "HUNHOUHNOHUOHOUH", "NHUNHOUTECUTTUUNOHUH", "NHONUHHOUTHOHHUTOCUUTUNT", "TOHUHUNHHUOUH", "THUOTNUHHOHUHOHUHTH", "HOUHHOTUHOHTUHHOUH", "OTUHOHUHTOHTUHTHOUHN"] ["UTHONUHNOTOu", "HUTNOEHUNHHueuhnh", "othutnouhUTNEHUHNHO", "ohuhohuhtUHEOUHNTONHeott", "TUNHENTUHuehu", "uhohntuhHUHENUTNHNH", "THUTNEHHteuhehUTEH", "TNUHTNETHNHENHUHNUHEN"]
--["TEONTUthuo", "toutuhHEHUNHUH", "tocontahurc", "hotnuhonuhc", "egucroegur", "thTHUERCNTOHUTH", "cgurohtnhou43234", "houhoeu9903.ouhnhuhh"] ["nuhontuhgcr", "outhoenthuHUHNE", "ohuntoccr", "onhunRCGRG", "ecug324324", "oguroe123123hnhu", "oeuhntohenuhn3324"]