
# Parte Teórica
## O que é a biblioteca/framework SCOTTY
Scotty é um framework/biblioteca que nos permite criar aplicações web utilizando Haskell. Com ele, podemos definir rotas e executar funções. Ele é inspirado no framework **Sinatra**, uma biblioteca focada no desenvolvimento web e na criação de APIs, porém na linguagem **Ruby**.

### Exemplo
```
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

main :: IO ()
main = scotty 3000 $ do -- Inicializa o servidor web na porta 3000
  middleware logStdoutDev -- Trata requisições do cliente
 -- Definição das rotas
  get "/hello" $ do 
    text "Hello, Haskell Web Service!"

```

## Trabalho da Isadora
O trabalho dela consiste em um jogo em que o jogador deve adivinhar a música pela letra. Abaixo, separei algumas partes que achei interessantes sobre o trabalho.

### Criação de tipos
Aqui, ela cria um novo tipo de dados, com o id da letra, o verso que aparecerá no jogo e o nome da música, que determinará a resposta correta.
```
data Lyric = Lyric {
      idLyric :: Int
    , verse :: String
    , song  :: String
    }deriving(Show, Generic)
```

### Verificação da resposta
Função muito importante que verifica se a resposta do usuário está correta.
```
checkAnswer :: String -> Int -> [Lyric] -> Bool
checkAnswer userAnswer idL lyrics = (map toLower userAnswer) == map toLower (song ((getAnswer lyrics idL)!!0))
```
# Parte Prática
Abaixo veremos alguns exemplos de códigos que utilizam a biblioteca Scotty
### Exemplo 01
Código que dá conselhos aleatórios baseado em uma lista de conselhos
```
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)

advices :: [Text]
advices =
    [ "It always seems impossible, until it's done."
    , "To cleanly remove the seed from an Avocado, lay a knife firmly across it, and twist."
    , "Fail. Fail again. Fail better."
    , "Play is the true mother of invention."
    , "Remedy tickly coughs with a drink of honey, lemon and water as hot as you can take."
    ]

getRandomAdvice :: IO Text
getRandomAdvice = do
    index <- randomRIO (0, length advices - 1)
    return $ advices !! index

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    get "/advice" $ do
        randomAdvice <- liftIO getRandomAdvice   
        text randomAdvice
```
### Vídeo

https://github.com/user-attachments/assets/47990084-0d7c-4b9e-a6d4-a35b9d619d59

### Exemplo 02
O conceito do programa é o mesmo do Exemplo 01, a única diferença é a saída, que ele empacota o conselho em um JSON
### Vídeo

https://github.com/user-attachments/assets/0aeb2709-95a3-46a0-8a02-060b5c61de2d

### Exemplo 03
Esse é o maior, define uma lista de pontos de interesse da UFSM, além de possuir mais de uma rota, cada uma delas tem sua saída.

### Rota "/poi"
```
 get "/poi" $ do
        -- We set the header manually because we are returning a text (manually formatted as json)
        setHeader "Content-Type" "application/json"
        let response = poiToJSONString ("Restaurante Universitário 2", -29.71400, -53.71937)
        text (pack response)
```
Retorna um ponto de interesse fixo, nesse caso, o RU2.

### Rota "/poilist"
```
 get "/poilist" $ do
        setHeader "Content-Type" "application/json"
        let response = poiListToJSONString poiList
        text (pack response)  
```
Retorna uma lista em JSON com todos pontos de interesse cadastrados.

### Rota "/near/:lat/:lon"
```
 get "/near/:lat/:lon" $ do
        setHeader "Content-Type" "application/json"
        givenLat <- pathParam "lat" :: ActionM Double
        givenLon <- pathParam "lon" :: ActionM Double
        let nearDistance = 1.5::Double
            near = filter isNear poiList
                where
                    distance (_, poiLat, poiLon) = calcDistance givenLat givenLon poiLat poiLon
                    isNear poi = distance poi <= nearDistance
            response = poiListToJSONString near
        text (pack response)
```
Por meio da URL, são passadas coordenadas e serão retornados os pontos de interesse que estiverem a até 1,5 km de distância. Esse valor pode ser alterado pelo usuário.

## Vídeo

https://github.com/user-attachments/assets/dcda66ad-4e3f-459f-8750-79544c1abec7


## Referencias

 - [1] - https://hackage.haskell.org/package/scotty
 - [2] - https://liascript.github.io/course/?https://raw.githubusercontent.com/AndreaInfUFSM/elc117-2024b/main/projects/scotty/README.md#1
 - [3] - https://hackage.haskell.org/package/wai
 - [4] - https://github.com/elc117/perso-2024b-fennerspohr (trabalho da isadora)




