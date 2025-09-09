
# Parte Teórica
## O que é a biblioteca/framework SCOTTY
Scotty é um framework/biblioteca que nos permite criar aplicações web utilizando haskell, com ele podemos definir rotas e executar funções. Ele é inspirado no framework "Sinatra", uma biblioteca focada no desenvolvimento web e criação de API's, só que na linguagem "Ruby". 

### Exemplo
```
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

main :: IO ()
main = scotty 3000 $ do -- Inicializa o servidor web na porta 3000
  middleware logStdoutDev -- Trata requisições do cliente

  -- Define your routes and handlers here
  get "/hello" $ do 
    text "Hello, Haskell Web Service!"

```

## Trabalho da Isadora
O trabalho dela consiste em um jogo onde o jogador deve advinhar a música pela letra, abaixo separei algumas partes que achei interessante sobre o trabalho.

### Criação de tipos
Aqui ela cria um novo tipo de dados, com o id da letra, o verso que aparecerá no jogo e o nome da música que decidira a resposta correta.
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
Aqui veremos alguns exemplos de códigos que utilizam a biblioteca Scotty
### Exemplo 01
Código que dá conselhos aleatórios baseado em uma lista de conselhos
```



# Exemplos

