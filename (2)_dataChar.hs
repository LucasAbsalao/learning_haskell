import Data.Char


offset :: Int -- Recomendado
offset = ord 'A' - ord 'a'


maiuscula :: Char -> Char
maiuscula ch = chr (ord ch + offset)


ehDigito :: Char -> Bool
ehDigito ch = (ch>='0')&&(ch<='9')


toEnum :: Char -> Int
fromEnum :: Int -> Char


offset2 = toEnum 'A' - toEnum 'a'
maiusculaNoImport :: Char -> Char
maiusculaNoImport ch = fromEnum (toEnum ch + offset2)


-- "abc" ++ "def" = abcdef
-- 'a' : "bcd" = abcd
-- show 3 = "3"
-- putSrtLn -> /n vai funcionar!!!
-- (read "3")::Int   as vezes especificar o tipo não é necessário
