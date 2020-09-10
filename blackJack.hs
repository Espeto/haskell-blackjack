import Data.Char
import System.IO
import System.Random
import Data.Array.IO
import Control.Monad

data Naipe = Copas | Espada | Paus | Ouro
    deriving(Eq,Show, Enum)

data Numero = Um | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | J | Q | K | A
    deriving(Eq,Show, Enum)

data Carta = CCarta Naipe Numero
    deriving(Show)

type Baralho = [Carta]
type Hand = [Carta]

embaralha :: [a] -> IO [a]
embaralha xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj

    where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs = newListArray (1,n) xs


-- Funções que geram a lista de tipos

generateEnumValues :: (Enum a) => [a]
generateEnumValues = enumFrom (toEnum 0)

naipeList :: [Naipe]
naipeList = generateEnumValues

numList :: [Numero]
numList = generateEnumValues

-- Gera uma carta a partir de um naipe e um número
makeCard :: Naipe -> Numero -> Carta
makeCard s n = CCarta s n


-- Gera todas as cartas de um naipe
makeNaipeCards :: (Naipe -> Numero -> Carta) -> [Numero] -> Naipe -> Baralho
makeNaipeCards f [] s = []
makeNaipeCards f (x:xs) s = f s x : makeNaipeCards f xs s 


-- Gera um baralho ja embaralhado
geraBaralho :: IO Baralho
geraBaralho = embaralha (geraB naipeList numList)

    where
        geraB :: [Naipe] -> [Numero] -> Baralho
        geraB [] nlist = []
        geraB (s:ss) nlist = geraBN s nlist ++ geraB ss nlist
        geraBN :: Naipe -> [Numero] -> Baralho
        geraBN s nlist = makeNaipeCards makeCard nlist s


-- Tira uma carta do baralho
-- OBS: Chamar antes a função pegaCartaBaralho para não perder a carta
tiraCartaBaralho :: Baralho -> Baralho
tiraCartaBaralho [] = []
tiraCartaBaralho (x:xs) = xs

pegaCartaBaralho :: Baralho -> Hand -> Hand
pegaCartaBaralho [] h = h
pegaCartaBaralho (x:xs) h = addToHand x h


-- Gera uma nova mão
genHand :: Hand
genHand = []

addToHand :: Carta -> Hand -> Hand
addToHand c h = c:h 

{- getHandPoints :: IO Baralho -> Baralho
getHandPoints  = n -}


{- printTable :: Baralho -> Hand -> Hand -> IO ()
printTable b ph dl = do -}


printCard :: Carta -> IO ()
printCard (CCarta s Dez) = do
    putStrLn (" _______\n\
        \|" ++ pN Dez ++ "     |\n\
        \|  ---  |\n\
        \| | " ++ pS s ++ " | |\n\
        \|  ---  |\n\
        \|_____" ++ pN Dez ++ "|\n")

printCard (CCarta s n) = do
    putStrLn (" _______\n\
            \|" ++ pN n ++"      |\n\
            \|  ---  |\n\
            \| | " ++ pS s ++ " | |\n\
            \|  ---  |\n\
            \|______"++ pN n ++ "|\n")

pS :: Naipe -> String
pS Copas = "\x2665"
pS Espada = "\x2660"
pS Paus = "\x2663"
ps Ouro = "\x2666"

pN :: Numero -> String
pN Um = show 1
pN Dois = show 2
pN Tres = show 3
pN Quatro = show 4
pN Cinco = show 5
pN Seis = show 6
pN Sete = show 7
pN Oito = show 8
pN Nove = show 9
pN Dez = show 10
pN n = show n


-- main :: IO ()
-- main = do
