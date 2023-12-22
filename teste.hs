import Cp
import List
import Data.List (transpose)

type Matrix a = [[a]]

-- Anamorfismo para a função espiral
spiral :: Matrix a -> [a]
spiral = anaList gSpiral

-- Função de desdobramento para o anamorfismo da espiral
gSpiral :: Matrix a -> Either () ([a], Matrix a)
gSpiral [] = i1 ()
gSpiral (x:xs) = i2 (x, rotate xs)

-- Função para rotacionar a matriz 90 graus no sentido anti-horário
rotate :: Matrix a -> Matrix a
rotate = reverse . transpose

-- Anamorfismo para listas usando a estrutura fornecida
anaList :: (b -> Either () (a, b)) -> b -> [a]
anaList g = inList . recList (anaList g) . g

-- Teste da função
main :: IO ()
main = do
    let mat1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    let mat2 = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]
    print $ spiral mat1
    print $ spiral mat2
