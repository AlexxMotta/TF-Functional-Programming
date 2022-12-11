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


------- Questões do jorge | 3 e 4
--Roupa (nome, cor, tamanho, preco,cat)
--Calcado (nome, material, tamanho, preco,cat)
--Acessorio (nome, preco,cat)

lista :: [Loja]
lista = [item1, item2, item3]

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
    | n < (length loja) = do
        (exibirItem ( loja !! n ))
        exibirItens loja (n+1)
    | otherwise = putStrLn("FIM")

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

exibirTodosAcessorios :: [Loja] -> IO()
exibirTodosAcessorios [] = putStrLn "Nao ha acessorios no estoque"
exibirTodosAcessorios items = putStrLn ((gerarCabecalhoTabelaAcessorios) ++ (gerarTabelaAcessorios items))

gerarCabecalhoTabelaAcessorios :: String
gerarCabecalhoTabelaAcessorios =  
    "|" 
    ++ (centralizarTexto (length "Listagem de Acessorios") 45 "Listagem de Acessorios") 
    ++ "|\n" 
    ++ "| " 
    ++ (centralizarTexto (length "Nome") 20 "Nome") 
    ++ " | " 
    ++ (centralizarTexto (length "Preco") 20 "Preco") 
    ++ " |\n"

gerarTabelaAcessorios :: [Loja] -> String
gerarTabelaAcessorios [] = ""
gerarTabelaAcessorios (primeiroItem : itensRestantes) = (gerarLinhaTabelaAcessorios primeiroItem) ++ (gerarTabelaAcessorios itensRestantes)

gerarLinhaTabelaAcessorios :: Loja -> String
gerarLinhaTabelaAcessorios (Acessorio nome preco _) = 
  "| " 
    ++ (centralizarTexto (length nome) 20 nome) 
    ++ " | " 
    ++ (centralizarTexto (length strPreco) 20 strPreco) ++ " |\n"
      where 
        strPreco = show preco

gerarLinhaTabelaAcessorios (Roupa _ _ _ _ _) = ""
gerarLinhaTabelaAcessorios (Calcado _ _ _ _ _) = ""


exibirTodosCalcados :: [Loja] -> IO()
exibirTodosCalcados [] = putStrLn "Nao ha calcados no estoque"
exibirTodosCalcados items = putStrLn ((gerarCabecalhoTabelaCalcados) ++ (gerarTabelaCalcados items))

gerarCabecalhoTabelaCalcados :: String
gerarCabecalhoTabelaCalcados =  
    "|" 
    ++ (centralizarTexto (length "Listagem de Calcados") 91 "Listagem de Calcados") 
    ++ "|\n" 
    ++ "| " 
    ++ (centralizarTexto (length "Nome") 20 "Nome") 
    ++ " | " 
    ++ (centralizarTexto (length "Material") 20 "Material") 
    ++ " | " 
    ++ (centralizarTexto (length "Tamanho") 20 "Tamanho") 
    ++ " | " 
    ++ (centralizarTexto (length "Preco") 20 "Preco") ++ " |\n"

gerarTabelaCalcados :: [Loja] -> String
gerarTabelaCalcados [] = ""
gerarTabelaCalcados (primeiroItem : itensRestantes) = (gerarLinhaTabelaCalcados primeiroItem) ++ (gerarTabelaCalcados itensRestantes)

gerarLinhaTabelaCalcados :: Loja -> String
gerarLinhaTabelaCalcados (Calcado nome material tamanho preco _) = 
  "| " 
    ++ (centralizarTexto (length nome) 20 nome) 
    ++ " | " 
    ++ (centralizarTexto (length material) 20 material) 
    ++ " | " 
    ++ (centralizarTexto (length tamanho) 20 tamanho) 
    ++ " | " 
    ++ (centralizarTexto (length strPreco) 20 strPreco) ++ " |\n"
      where 
        strPreco = show preco

gerarLinhaTabelaCalcados (Roupa _ _ _ _ _) = ""
gerarLinhaTabelaCalcados (Acessorio _ _ _) = ""

imprimirEspacos :: Int -> String
imprimirEspacos 0 = ""
imprimirEspacos n = " " ++ imprimirEspacos (n - 1)

obterEspacoLateralAMais :: Int -> Int -> String
obterEspacoLateralAMais tamanhoMaximo tamanhoPalavra
  | odd diferencaTamanho = " "
  | otherwise = ""
    where 
      diferencaTamanho = tamanhoMaximo - tamanhoPalavra


centralizarTexto :: Int -> Int -> String -> String
centralizarTexto tamanhoPalavra tamanhoMaximo texto
  | tamanhoMaximo > length texto = espacoAMais ++ (imprimirEspacos totEspacosLaterais) ++ texto ++ (imprimirEspacos totEspacosLaterais)
  | tamanhoMaximo == length texto = texto
  | otherwise = "Erro: o texto não cabe no intervalo dado"
    where 
      diferencaTamanho = tamanhoMaximo - tamanhoPalavra
      totEspacosLaterais = div diferencaTamanho 2
      espacoAMais = obterEspacoLateralAMais tamanhoMaximo tamanhoPalavra

main = exibirTodosCalcados lista

-- listaValor :: [Loja] -> Int -> IO()
-- listaValor lista valor = exibirItens(filter (\n -> isExibirVal n valor) lista)

-- isExibirVal :: Loja -> Int -> Bool
-- isExibirVal (Roupa nome cor tamanho preco cat) x = preco == x
-- isExibirVal (Calcado nome material tamanho preco cat) x = preco == x
-- isExibirVal (Acessorio nome preco cat) x = preco == x

-- type ItemValorCategoria = (Float, String)

-- transformarLojaEmItemValorCategoria :: Loja -> ItemValorCategoria
-- transformarLojaEmItemValorCategoria (Calcado nome material tamanho preco categoria) = (preco, categoria)

-- pertenceCategoria :: ItemValorCategoria -> String -> Bool
-- pertenceCategoria (preco, categoria) categoriaBuscada = categoria == categoriaBuscada

-- obterPrecoItemValorCategoria :: ItemValorCategoria -> Float
-- obterPrecoItemValorCategoria (preco, categoria) = preco
 
-- obterValorTotalCategoria :: [Loja] -> String -> Float
-- obterValorTotalCategoria [] = 0
-- obterValorTotalCategoria (primeiroItem: itensRestantes) categoriaBuscada 
--   | pertenceCategoria (transformarLojaEmItemValorCategoria primeiroItem) = obterPrecoItemValorCategoria (transformarLojaEmItemValorCategoria primeiroItem) + obterValorTotalCategoria itensRestantes  
--   | not pertenceCategoria (transformarLojaEmItemValorCategoria primeiroItem) = 0 + obterValorTotalCategoria itensRestantes