module Main where


import Data.Char
import System.IO
import System.Random
import Data.Array.IO
import Control.Monad
import System.Console.ANSI
import System.IO

data Naipe = Copas | Espada | Paus | Ouro
    deriving(Eq,Show, Enum)

data Numero = Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | J | Q | K | A
    deriving(Eq,Show, Enum)

data Carta = CCarta Naipe Numero
    deriving(Eq, Show)

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

-----------------------------------------------------------------------------
------------- Até aqui são só as função para geração do baralho -------------
----------------------------------------------------------------------------- 


-----------------------------------------------------------------------------
------------------------ Manipulação do baralho -----------------------------
----------------------------------------------------------------------------- 
-- Tira uma carta do baralho
-- OBS: Chamar antes a função pegaCartaBaralho para não perder a carta
tiraCartaBaralho :: Baralho -> IO Baralho
tiraCartaBaralho baralho = do
    if (baralho == [])
        then return []
        else do
            nbaralho <- tiraCarta baralho
            return nbaralho

    where
        tiraCarta :: Baralho -> IO Baralho
        tiraCarta (x:xs) = return xs


pegaCartaBaralho :: Baralho -> Hand -> IO Hand
pegaCartaBaralho baralho hand = do
    if (baralho == [])
        then return hand
        else do
            nhand <- pegaCarta baralho hand
            return nhand

    where
        pegaCarta :: Baralho -> Hand -> IO Hand
        pegaCarta (x:xs) h = return (addToHand x h)
        addToHand :: Carta -> Hand -> Hand
        addToHand c h = c:h 


-----------------------------------------------------------------------------
------------------------ Manipulação do baralho -----------------------------
----------------------------------------------------------------------------- 


-- Gera uma nova mão
genHand :: IO Hand
genHand = return []


getHandPoints :: Hand -> Int
getHandPoints hand
    | checkAs hand && checkjqk hand = ((countAs hand) * 11) + (getPoints (removeAs hand))
    | otherwise = getPoints hand 

    where
        checkAs :: Hand -> Bool
        checkAs [] = False
        checkAs ((CCarta _ n):xs)
            | n == A = True
            | otherwise = checkAs xs
        checkjqk :: Hand -> Bool
        checkjqk [] = False
        checkjqk ((CCarta _ n):xs)
            | n == J || n == Q || n == K = True
            | otherwise = checkAs xs
        countAs :: Hand -> Int
        countAs [] = 0
        countAs ((CCarta _ n):xs)
            | n == A = 1 + countAs xs
            | otherwise = countAs xs
        removeAs :: Hand -> Hand
        removeAs [] = []
        removeAs ((CCarta s n):xs)
            | n /= A = (CCarta s n) : removeAs xs
            | otherwise = removeAs xs
        getPoints :: Hand -> Int
        getPoints [] = 0
        getPoints (x:xs) = getCardPoint x + getPoints xs
        getCardPoint :: Carta -> Int
        getCardPoint (CCarta _ A) = 1
        getCardPoint (CCarta _ K) = 10
        getCardPoint (CCarta _ Q) = 10
        getCardPoint (CCarta _ J) = 10
        getCardPoint (CCarta _ Dez) = 10
        getCardPoint (CCarta _ Nove) = 9
        getCardPoint (CCarta _ Oito) = 8
        getCardPoint (CCarta _ Sete) = 7
        getCardPoint (CCarta _ Seis) = 6
        getCardPoint (CCarta _ Cinco) = 5
        getCardPoint (CCarta _ Quatro) = 4
        getCardPoint (CCarta _ Tres) = 3
        getCardPoint (CCarta _ Dois) = 2


{- printCard :: Carta -> IO ()
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
            \|______"++ pN n ++ "|\n") -}

printTable :: Baralho -> Hand -> Hand -> IO ()
printTable b ph dh = do 
    putStrLn "\nCartas do Dealer"
    printFullHand dh
    putStrLn " "
    putStrLn "Suas Cartas"
    printFullHand ph
    putStrLn " "
    putStr "Qtd Cartas no Baralho: "
    putStrLn (show (countBaralho b) ++ "\n")

countBaralho :: Baralho -> Int
countBaralho [] = 0
countBaralho (x:xs) = 1 + countBaralho xs

printFullHand :: Hand -> IO ()
printFullHand [] = putStrLn "Vazio"
printFullHand hand = putStrLn (composeString hand)

    where
        composeString :: Hand -> String
        composeString [x] = printCard x
        composeString (x:xs) = printCard x ++ "  " ++ composeString xs
        printCard :: Carta -> String
        printCard (CCarta naipe num) = "("++ (pN num) ++ (printNaipe naipe) ++")"

printNaipe :: Naipe -> String
printNaipe Copas = "\x2665"
printNaipe Espada = "\x2660"
printNaipe Paus = "\x2663"
printNaipe Ouro = "\x2666"

pN :: Numero -> String
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

main :: IO ()
main = do
    hSetEcho stdin True
    deck <- geraBaralho
    d_hand <- genHand
    p_hand <- genHand
    gameLoop deck d_hand p_hand 0


gameLoop :: Baralho -> Hand -> Hand -> Int -> IO ()
gameLoop b dh ph dqtd = do
    putStrLn "\nMesa"
    printTable b ph dh
    putStrLn "Pegar uma carta?"
    pr <- getLine
    if (pr == "s")
        then do 
            nph <- pegaCartaBaralho b ph
            nb <- tiraCartaBaralho b
            printTable nb nph dh
            let playerP = (getHandPoints nph)
            if (playerP == 21)
                then do
                    putStrLn "VOCE VENCEU!"
                else do
                    if (playerP > 21)
                        then do
                            putStrLn "VOCE PERDEU!"
                        else do
                            if (dqtd < 5)
                                then do
                                    putStrLn "O Dealer vai tirar uma carta"
                                    ndh <- pegaCartaBaralho nb dh
                                    nnb <- tiraCartaBaralho nb
                                    printTable nnb nph ndh
                                    let dealerP = (getHandPoints ndh)
                                    if (dealerP > 21)
                                        then do
                                            putStrLn "VOCE VENCEU!"
                                        else gameLoop nnb ndh nph (dqtd+1)
                                else gameLoop nb dh nph dqtd
        else do
            if (dqtd < 5)
                then do
                    putStrLn "O Dealer vai tirar uma carta"
                    ndh <- pegaCartaBaralho b dh
                    nb <- tiraCartaBaralho b
                    printTable nb ph ndh
                    let dealerP = (getHandPoints ndh)
                    if (dealerP == 21)
                        then do
                            putStrLn "VOCE PERDEU!"
                        else do
                            if (dealerP > 21)
                                then do
                                    putStrLn "VOCE PERDEU!"
                                else gameLoop nb ndh ph (dqtd+1)
                else gameLoop b dh ph dqtd