--Roupa (nome, cor, tamanho, preco,cat)
--Calcado (nome, material, tamanho, preco,cat)
--Acessorio (nome, preco,cat)
data Loja = 
     Roupa String String String Float String
    |Calcado String String String Float String
    |Acessorio String Float String
    deriving (Eq,Ord,Show)

item1,item2,item3 :: Loja
item1 = Roupa "blusa regata" "branca" "M" 139.90 "roupa"
item2 = Calcado "tenis corrida" "tecido" "39" 329.90 "calcado"
item3 = Acessorio "brinco lua" 19.90 "acessorio"

-- 1.   Crie uma função que recebe uma lista de itens da Loja e mostre os detalhes de
-- todos os itens de forma legível.
-- 2.   Crie uma função que recebe uma lista de itens da Loja e mostre uma lista das
-- roupas do estoque.

--Roupa (nome, cor, tamanho, preco,cat)
--Calcado (nome, material, tamanho, preco,cat)
--Acessorio (nome, preco,cat)

exibirItem :: Loja -> IO()
exibirItem (Roupa nome cor tamanho preco cat) = do
    putStrLn "------ ROUPA ------"
    putStrLn ("Nome: " ++ show nome)
    putStrLn ("Cor: " ++ show cor)
    putStrLn ("Tamanho: " ++ show tamanho)
    putStrLn ("Preco: " ++ show preco)
    putStrLn ("Cat: " ++ show cat)

exibirItem (Calcado nome material tamanho preco cat) = do
    putStrLn "------ CALCADO ------"
    putStrLn ("Nome: " ++ show nome)
    putStrLn ("Material: " ++ show material)
    putStrLn ("Tamanho: " ++ show tamanho)
    putStrLn ("Preco: " ++ show preco)
    putStrLn ("Cat: " ++ show cat)

exibirItem (Acessorio nome preco cat) = do
    putStrLn "------ ACESSORIO ------"
    putStrLn ("Nome: " ++ show nome)
    putStrLn ("Preco: " ++ show preco)
    putStrLn ("Cat: " ++ show cat)


exibirItens :: [Loja] -> Int -> IO()
exibirItens loja n
    |n < (length loja) = do
        (exibirItem (loja!!n))
        exibirItens loja (n+1)
    |otherwise = putStrLn("FIM")

exibirApenasRoupas :: Loja -> IO()
exibirApenasRoupas (Roupa nome cor tamanho preco cat) = do
    putStrLn "------ ROUPA ------"
    putStrLn ("Nome: " ++ show nome)
    putStrLn ("Cor: " ++ show cor)
    putStrLn ("Tamanho: " ++ show tamanho)
    putStrLn ("Preco: " ++ show preco)
    putStrLn ("Cat: " ++ show cat)

exibirRoupas :: [Loja] -> Int -> IO()
exibirRoupas loja n
    |n < (length loja) = do
        (exibirApenasRoupas (loja!!n))
        exibirRoupas loja (n+1)
    |otherwise = putStrLn("FIM")
