[Sumário](index)

## Capítulo 5. Escrevendo uma biblioteca para dados no formato JSON

### Um tour rápido pelo JSON

Neste capítulo, vamos desenvolver uma pequena, mas completa, biblioteca Haskell. Nossa biblioteca manipulará e serializará dados em uma  popular formato conhecido como JSON.

A linguagem JSON (JavaScript Object Notation) é uma representação pequena e simples para armazenar e transmitir dados estruturados, por exemplo, por meio de uma conexão de rede. É mais comumente usado para transferir dados de um serviço da Web para um aplicativo JavaScript baseado em navegador. O formato JSON é descrito em [www.json.org](http://www.json.org/), e em maior detalhe por [RFC 4627](http://www.ietf.org/rfc/rfc4627.txt).

O JSON suporta quatro tipos básicos de valor: strings, numbers, booleans e um valor especial chamado `null`.

```
"a string" 12345 true
      null
```

A linguagem fornece dois tipos compostos: um _array_ é uma sequência ordenada de valores e um _object_ é uma coleção não ordenada de pares nome / valor. Os nomes em um objeto são sempre strings; os valores em um objeto ou matriz podem ser de qualquer tipo.

```
[-3.14, true, null, "a string"]
      {"numbers": [1,2,3,4,5], "useful": false}
```

### Um tour rápido pelo Stack

(AQUI VOU DESVCREVER UM TUTORIAL RAPIDO DE STACK)

### Representing JSON data in Haskell

Primeiro, crie um novo arquivo SimpleJSON em src:

```haskell
-- src/SimpleJSON.hs
module SimpleJSON where
```

Para trabalhar com dados JSON no Haskell, usamos um tipo de dados algébricos para representar os valores possíveis nesse formato:

```haskell
-- src/SimpleJSON.hs
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)
```

Para cada tipo de JSON, fornecemos um construtor de valor distinto. Alguns desses construtores possuem parâmetros: se quisermos construir uma string JSON, devemos fornecer um valor String como um argumento para o construtor `JString`.

Para começar a experimentar esse código, salve o arquivo `SimpleJSON.hs` no seu editor, alterne para uma janela de terminal e carregue o arquivo executando o seguinte comando:

```
$ stack ghci
Using main module: 1. Package `hs2json' component exe:hs2json-exe with main-is file: /home/sergiosouzacosta/tmp/hs2json/app/Main.hs
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: hs2json
GHCi, version 8.6.3: http://www.haskell.org/ghc/  :? for help
[1 of 3] Compiling Lib              ( /home/sergiosouzacosta/tmp/hs2json/src/Lib.hs, interpreted )
[2 of 3] Compiling Main             ( /home/sergiosouzacosta/tmp/hs2json/app/Main.hs, interpreted )
[3 of 3] Compiling SimpleJSON       ( /home/sergiosouzacosta/tmp/hs2json/src/SimpleJSON.hs, interpreted )
Ok, three modules loaded.
*Main Lib SimpleJSON> JString "foo"
JString "foo"
*Main Lib SimpleJSON>  JNumber 2.7
JNumber 2.7
*Main Lib SimpleJSON> :type JBool True
JBool True :: JValue
```

Podemos ver como usar um construtor para obter um valor Haskell normal e transformá-lo em um JValue. Para fazer o inverso, usamos casamento de padrões. Aqui está uma função que podemos adicionar ao `SimpleJSON.hs` que irá extrair uma string de um valor JSON para nós. Se o valor JSON realmente contiver uma string, nossa função envolverá a string com o construtor `Just`. Caso contrário, ele retornará `Nothing`.

```haskell
-- src/SimpleJSON.hs
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing
```
Quando salvamos o arquivo de código-fonte modificado, podemos recarregá-lo no **stack ghci ** e testar a nova definição:

```
*Main Lib SimpleJSON> :r
[3 of 3] Compiling SimpleJSON       ( /home/sergiosouzacosta/tmp/hs2json/src/SimpleJSON.hs, interpreted )
Ok, three modules loaded.
*Main Lib SimpleJSON> getString (JString "hello")
Just "hello"
*Main Lib SimpleJSON> getString (JNumber 3)
Nothing
```
A seguir mais algumas funções acessoras:

```haskell
-- src/SimpleJSON.hs
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v            = v == JNull
```
A função `truncate` transforma um ponto flutuante ou número racional em um inteiro, soltando os dígitos após o ponto decimal.

```
*Main Lib SimpleJSON> truncate 5.8
5
*Main Lib SimpleJSON> :module +Data.Ratio
*Main Lib SimpleJSON Data.Ratio> truncate (22 % 7)
3
```

### A anatomia de um módulo Haskell

Um arquivo fonte do Haskell contém uma definição de um único _module_. Um módulo nos permite determinar quais nomes dentro do módulo são acessíveis a partir de outros módulos.

Um arquivo fonte começa com uma declaração _module_. Isso deve preceder todas as outras definições no arquivo de fonte.

```haskell
-- file: src/SimpleJSON.hs
module SimpleJSON
    (
      JValue(..)
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
    ) where
```
A palavra `module` é reservada. Ela é seguida pelo nome do módulo, que deve começar com uma letra maiúscula. Um arquivo de origem deve ter o mesmo _base name_ (o componente antes do sufixo) como o nome do módulo que ele contém. É por isso que nosso arquivo `SimpleJSON.hs` contém um módulo chamado` SimpleJSON`.

Após o nome do módulo, há uma lista de _export_, entre parênteses. A palavra-chave `where` indica que o corpo do módulo segue.

A lista de exportações indica quais nomes neste módulo estão visíveis para outros módulos. Isso nos permite manter o código privado escondido do mundo exterior. A notação especial `(..)` que segue o nome `JValue` indica que estamos exportando o tipo e todos os seus construtores.

Pode parecer estranho que possamos exportar o nome de um tipo (ou seja, seu construtor de tipo), mas não seus construtores de valor. A capacidade de fazer isso é importante: nos permite ocultar os detalhes de um tipo de seus usuários, fazendo o tipo _abstract_. Se não pudermos ver os construtores de valor de um tipo, não poderemos combinar o padrão com um valor desse tipo, nem podemos construir um novo valor desse tipo. Mais adiante neste capítulo, discutiremos algumas situações em que podemos querer usar um tipo abstrato.

Se omitirmos as exportações (e os parênteses que as envolvem) de uma declaração do módulo, todos os nomes no módulo serão exportados.


```haskell
-- file: src/SimpleJSON.hs
module ExportEverything where
```
Para exportar nem um nome (o que raramente é útil), nós escrevemos uma lista vazia de exportação usando um par de parêntesis.

```haskell
-- file: src/SimpleJSON.hs
module ExportNothing () where
```

### Compilando um programa Haskell

Para compilar um arquivo de código-fonte e executar o binário, primeiro abrimos um terminal ou janela de prompt de comando, depois invocamos os seguintes comandos:

```
$ stack build
$ stack run
someFunc
```

Agora que compilamos com sucesso nossa biblioteca mínima, vamos começar a escrever a biblioteca proposta aqui. Então, antes de seguir, apague o arquivo Lib.hs, já que não iremos usar mais e então modifique o arquiv app/Main.hs: 

```haskell
-- file: app/Main.hs
module Main (main) where

import SimpleJSON

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
```
Observe a diretiva `import` que segue a declaração do módulo. Isso indica que queremos pegar todos os nomes que são exportados do módulo `SimpleJSON` e disponibilizá-los em nosso módulo. Quaisquer diretivas `import` devem aparecer em um grupo no início de um módulo. Eles devem aparecer após a declaração do módulo, mas antes de todos os outros códigos. Nós não podemos, por exemplo, espalhá-los através de um arquivo fonte. 

O nome dos arquivos fontes e das funções são a cargo do programador. Porém, para criar um executável, o ** ghc ** espera um módulo chamado `Main` que contenha uma função chamada` main`. A função `main` é aquela que será chamada quando rodarmos o programa assim que o construirmos.

```
$stack buid
$ stack run
JObject [("foo",JNumber 1.0),("bar",JBool False)]
```


### Imprimindo dados JSON


Agora que temos uma representação em Haskell para os tipos JSON, nós gostaríamos de ser capazes de pegar valores em Haskell e processá-los como dados JSON.

Há algumas maneiras que podemos fazer isso. Talvez, a mais direta seja escrever uma função que imprima os valores no formato JSON. Quando terminarmos, iremos explorar outras abordagens interessantes. 

```haskell
-- file: src/PutJSON.hs
module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v)   = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)
```

Uma boa prática em Haskell envolve separar o código puro do código que produz uma saída do tipo `IO ()`. Nossa função `renderJValue` não tem interação com o mundo exterior, mas ainda precisamos ser capazes de imprimir um ****JValue*.

```haskell
-- file: src/PutJSON.hs
putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
```

Imprimir um valor JSON é fácil agora.

Por que devemos separar o código de renderização do código que, realmente, imprime um valor? Isto nos dá flexibilidade. Por exemplo, se quisermos compactar os dados antes de imprimi-lo e misturamos o código de renderização com o de impressão, ficaria muito mais difícil adaptar nosso código, nessa circunstância.

Essa ideia de separar o código puro do código impuro é poderosa e universal no código Haskell. Várias bibliotecas de compressão existe, todas têm uma simples interface: um função de compressão que aceita uma string descompactada e retorna uma string compactada. Nós podemos usar a função de composição para converter dados em JSON para string e compactar para outra string, postergando qualquer decisão de como, efetivamente, mostrar ou transmitir os dados.

Experimentando
```
$stack ghci
*Main PutJSON SimpleJSON> putJValue (JObject [("nome", JString "Sergio"), ("idade", JNumber 38)])
{"nome": "Sergio", "idade": 38.0}
```


#### Uma visão mais geral de renderização

Nosso código de renderização JSON está adaptado as necessidades do nossos tipos de dados e as convenções de formatação JSON. A saída que ele produz pode não ser amigável aos olhos humanos. agora nós iremos olhar renderização como uma tarefa mais genérica: como podemos construir uma biblioteca útil para renderizar dados em uma variedade de situações?

Nós gostaríamos de produzir saídas que são adequadas ou para consumo humano (para debugar, por exemplo) ou para processamento. Bibliotecas que fazem essa tarefa são chamadas de _pretty printers_. Há prontas várias bibliotecas Haskell desse tipo. Nós estamos criando a nossa não para substitui-las, mas por os vários aprendizados que ganharemos em desing de bibliotecas e técnicas de programação funcional.

Nós iremos chamar nosso genérico módulo _pretty printers_ como `Prettify`, então nosso código estará no arquivo chamado `Prettify.hs`.

![[Note]](assets/note.png)

Nomeando

No nosso `Prettify` módulo, nós iremos basear nossos nomes naqueles usados por várias bibliotecas bem estabelecidas desse tipo. Isso nos dará um grau de compatibilidade com as bibliotecas mais maduras.

Para termos certeza que `Prettify` atende às necessidades práticas, iremos escrever um novo renderizador de JSON que use a API `Prettify`. Depois que estiver pronto, nós voltaremos e entramos em detalhes do `Prettify` módulo. 

Ao invés de renderizar direto para string, nosso `Prettify` irá usar um tipo abstrato, que chamaremos de Doc. Baseando-se nossa biblioteca em um tipo abstrato, nó podemos escolher uma implementação flexível e eficiente. Se decidirmos mudar o código sobreposto, nossos usuários não serão capazes de relatar.

Nós iremos chamar nosso renderizador JSON de `PrettyJSON.hs`, e manter o nome `renderJValue` para a função de renderização. Renderizar um dos valores básicos do JSON é simples.

```haskell
-- file: ch05/PrettyJSON.hs
module PrettyJSON where

import SimpleJSON
import Prettify

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
```

O tipo Doc, e as funções, `text`, `double`, e `string` serão fornecidas pelo nosso `Prettify` módulo.

#### Desenvolvendo código Haskell sem quebrar a cabeça

Desdo início, quando nos familiarizamos com o desenvolvimento em Haskell, nós temos muitos conceitos novos e desconhecidos para entender de uma vez que pode ser um desafio escrever código que compile sem erros.

Enquanto escrevemos o corpo inicial do código, é uma grande ajuda parar a cada poucos minutos e tentar compilar o código que produzimos até o momento. Por Haskel ser fortemente tipado, se o código compilar corretamente, estamos assumindo que estamos longe das armadilhas da programação.

Uma forma útil para desenvolver o esqueleto de um programa é escrever espaços reservados ou versões de esboço de nossos tipos e funções. Por exemplo, nós mencionamos acima que as funções, `string`, `text` e `double` serão escritas  no nosso `Prettify` módulo, se nós não fornecermos a definição dessas funções ou do tipo Doc, nosso lema "compile cedo, compile frequentemente" irá falhar no nosso renderizador, como o compilador não conhece nada sobre essas funções. Para evitar esse problemas nós escrevemos código de esboço que não faz nada.

```haskell
-- file: src/Prettify.hs
module Prettify where

import SimpleJSON

data Doc = ToBeDefined
         deriving (Show)

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined
```

O valor especial `undefined` tem o tipo `a`, então não há erro de verificação de tipos, não importa onde o usamos. Se tentarmos valora-lo, isso causará um erro em nosso programa.

```
ghci> :type undefined
undefined :: a
ghci> undefined
*** Exception: Prelude.undefined
ghci> :type double
double :: Double -> Doc
ghci> double 3.14
*** Exception: Prelude.undefined
```
Embora não podemos executar nosso esboço o verificador de tipos do compilador garantirá que nosso programa foi sensivelmente tipado.

#### Impressão agradável de uma string


Quando precisamos imprimir uma string, o JSON envolve moderadamente as regras  de escape que devemos seguir. No nível mais alto, uma string é somente uma série de caracteres entre aspas.

```haskell
-- file: src/Prettify.hs
string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar
```

![[Note]](assets/note.png)

Estilo ponto livre

Este estilo de escrever uma definição exclusivamente como uma composição de outras funções é chamado de estilo ponto livre. O uso da palavra "ponto" não e em menção ao carácter "`.`" usado para composição de funções. Este termo é aproximadamente sinônimo (em Haskell) de valor, então uma expressão de ponto livre não faz menção ao valor que ela opera.

Compare a definição de `string` (com ponto livre) a cima com a versão "pointy" abaixo, a qual usa a váriavel `s` para se referir ao valor em que opera.

```haskell
-- file: src/PrettyJSON.hs
pointyString :: String -> Doc
pointyString s = enclose '"' '"' (hcat (map oneChar s))
```
A função `enclose` simplesmente põe um valor Doc entre um carácter de abertura e um de fechamento.

```haskell
-- file: src/PrettyJSON.hs
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right
```
Nós forneceremos a função `(<>)` em nossa biblioteca `Prettify`. Ele concatena dois Doc valores, então, ele é equivalente a função `(++)`.

```haskell
-- file: src/Prettify.hs
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined
```

Para evitar conflito com o operador `<>` já existente em Prelude, uma alternativa é esconder esse operador na importação:

```haskell
import Prelude hiding ((<>))
```

Nossa biblioteca `Prettify` também fornece `hcat`, que concatena múltiplos valores Doc em um só, é análogo ao `concat` para listas 

```haskell
-- file: src/Prettify.hs
hcat :: [Doc] -> Doc
hcat xs = undefined
```
Nossa função `string` aplica a função `oneChar` para todos os caracteres de um string, concatena em lote, e põe o resultado entre aspas. A função `oneChar` escapa ou renderiza um carácter individual.

```haskell
-- file: src/PrettyJSON.hs
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])
```

O valor `simpleEscapes` é uma lista de pares. Nós chamamos uma lista de pares de associação de listas, ou simplesmente _alist_(do inglês _association list_). Cada elemento da nossa _alist_ associa um carácter a sua versão de escape.


```
ghci> take 4 simpleEscapes
[('\b',"\\b"),('\n',"\\n"),('\f',"\\f"),('\r',"\\r")]
```

Nossa expressão `case` tenta ver se nosso carácter casa com a _alist_. Se encontrarmos uma correspondência nós o emitimos, caso contrário, talvez nós precisamos escapar o carácter de uma forma mais complicada. Nesse caso, realizamos esse escape. Somente se nenhum tipo de escapamento é necessário nós emitimos como texto plano. Para ser conservador, os únicos carácter sem escape que emitiremos são caracteres ASCII imprimíveis.

O escapamento mais sofisticado envolve transformar o carácter na string “`\u`”  seguida por a uma sequência de quatro carácteres hexadecimais representando o valor numérico do Unicode carácter.

```haskell
-- file: src/PrettyJSON.hs
smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
    where h = showHex x ""
```
Para usar o showHex é preciso importar:
```
import Numeric (showHex)
```
A função `showHex` vem da biblioteca `Numeric` (você irá precisar importá-lo no início de `Prettify`), e retorna a representação hexadecimal de um número.

Em outro terminal e em outra pasta, fora do projeto:
```
$stack ghci
Prelude> :module Numeric
Prelude Numeric> showHex 114111 ""
"1bdbf"
```
A função `replicate` é providenciada pelo Prelude, e cria uma lista repetida de tamanho fixo, o tamanho é definido pelo seu argumento 
```
*Main> replicate 5 "foo"
["foo","foo","foo","foo","foo"]
```

Há um problema: a codificação de quatro dígitos fornecida pelo `smallHex` pode representar somente caracteres unicode até `0xffff`. Caracteres Unicode válidos podem ir até `0x10ffff`. Para representar adequadamente um carácter acima de `0xffff` em uma string JSON, nós seguimos algumas regras complicadas para dividir a string em dois. Isto nos dá a oportunidade de executar algumas manipulações a nível de bit dos números Haskell.

```haskell
-- file: src/PrettyJSON.hs
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n \`shiftR\` 10) .&. 0x3ff
          b = n .&. 0x3ff
```

A função `shiftR` é fornecida pelo módulo `Data.Bits`, e descola um número a direita. A função `(.&.)` (operador _e_), também de `Data.Bits`, executa uma conjunção binária a nível de bit em dois valores.

```
$stack ghci
Prelude> :module Data.Bits
Prelude Data.Bits>  0x10000 `shiftR` 4
4096
```
Agora que escrevemos `smallHex` e `astral`, nós podemos fornecer a definição para `hexEscape`.

```haskell
-- file: src/PrettyJSON.hs
hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c
```

Ok, agora pode compilar:

```
$ stack build
```

### Arrays, objetos, e o módulo cabeçalho


Comparado com strings, impressão agradável de arrays e objetos é fácil. Nós sabemos que ambos são visualmente similar: cada um inicia com um carácter de abertura, seguido por uma série de valores separados com vírgulas, seguido por um carácter de fechamento. Vamos escrever uma função que captura a estrutura comum de arrays e objetos. 

```haskell
-- file: src/PrettyJSON.hs
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item
```
Começaremos interpretando o tipo dessa função. Ela recebe um carácter de abertura e fechamento, e uma função que sabe como imprimir um valor de algum tipo desconhecido `a`, seguido por uma lista de valores do tipo `a`, e retorna um valor do tipo `Doc`.

Note que embora nossa assinatura de tipos mencione quatro parâmetros, nós listamos apenas três na definição da função. Nós estamos simplesmente seguindo a mesma regra que nos permite simplificar uma definição como `myLength xs = length xs` para `myLength = length`.

Nós já escrevemos `enclose`, que coloca um valor `Doc` entre um carácter de abertura e outro de fechamento. A função `fsep` estará no nosso módulo `Prettify`. Ela combina uma lista de valores `Doc` em um, possivelmente quebrando linhas caso a saída não caiba em uma linha.


```haskell
-- file: src/Prettify.hs
fsep :: [Doc] -> Doc
fsep xs = undefined
```
Apartir de agora, você poderá definir seus próprios esboços em `Prettify`, seguindo os exemplos que fornecemos. Não definiremos explicitamente mais nem um esboço.

A função `punctuate`  também será definida em nosso módulo `Prettify`, e podemos defini-la em termos de funções, as quais já escrevemos esboços. 

```haskell
-- file: src/Prettify.hs
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds  
```

Com essa definição de `series`, imprimir arrays é totalmente direto. Nós adicionamos essa equação no final do bloco que escrevemos para a função `renderJValue`. 

```haskell
-- file: src/PrettyJSON.hs
renderJValue (JArray ary) = series '\[' '\]' renderJValue ary
```
Para imprimir um objeto, nós precisamos fazer apenas um pequeno trabalho: parar cada elemento nós temos um nome e um valor para lidar.

```haskell
-- file: src/PrettyJSON.hs
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name,val) = string name
                          <> text ": "
                          <> renderJValue val
```

Ok, agora pode compilar:

```
$ stack build
```


### Escrevendo o módulo cabeçalho

Agora que escrevemos a estrutura do nosso arquivo `PrettyJSON.hs`, devemos voltar ao topo e adicionar a declaração do módulo. 

```haskell
-- file: src/PrettyJSON.hs
module PrettyJSON
    (
      renderJValue
    ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text,
                 compact, pretty)
```
Nós exportamos apenas uma função desse módulo: `renderJValue`, nossa função de renderização de JSON. AS outras defiinições do módulo existem puramente para dar suporte a `renderJValue`, então não há razão para faze-las visíveis a outros módulos.

A respeito das importações, os módulos `Numeric` e `Data.Bits` são distribuídos com o GHC. Nós já escrevemos o módulo `SimpleJSON`, e preenchemos nosso `Prettify` módulo com uma definição esquelética. Note que não há diferença na forma como importamos módulos padrões daqueles que escrevemos nós mesmos.

Em cada diretiva `import`, nós explicitamente listamos cada um dos nomes que queremos trazer para o escopo do nosso módulo. Isto não é obrigatório: Se omitirmos a lista de nomes, todos os nomes exportados do módulo serão disponíveis para nós. No entanto, é geralmente uma boa ideia explicitar a lista de importação.

*   Uma lista explicita deixa claro quais nomes nós estamos importando. Isto tornará fácil para o leitor buscar na documentação caso encontre uma função desconhecida.
    
*   Ocasionalmente, o mantedor de uma biblioteca irá remover ou renomear uma função. Se uma função desaparecer de um módulo de terceiros que usamos, qualquer erro de compilação resultante irá ocorrer tempo depois que escrevermos o módulo. A lista explícita de nomes importados pode agir como um lembrete para nós mesmos de onde estávamos importando o nome ausente, o que nos ajudará a identificar o problema maus rápido.


*   Pode ocorrer que alguém irá adicionar um nome a um módulo que é idêntico a um nome que está em seu código. Se não usarmos uma definição explícita, nós iremos terminar com o mesmo nome em nosso módulo, duas vezes. Se usarmos aquele nome, GHC irá reportar um erro devido a essa ambiguidade. Uma lista explicita nos permite evitar a possibilidade de importar, acidentalmente, um nome inesperado.

A ideia de explicitar importações é uma orientação que normalmente faz sentido, não uma regra rígida. Eventualmente, precisamos de tantos nomes de um módulo que listar cada um deles se torna chato. Em outros casos, um módulo pode ser tão largamente usado que um programador Haskell com experiência moderada sabe quais nomes vem do módulo.

### Criando o biblioteca de impressão agradável


Em nosso módulo `Prettify`, nós representamos o tipo Doc como um típo de dado algébrico.

```haskell
-- file: src/Prettify.hs
data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show,Eq)
```
Observe que o tipo Doc é, na verdade, uma árvore. Os contrutores `Concat` e `Union` criam um nó interno de outros dois valores Doc, enquanto `Empty` e outros construtores simples formam as folhas.

No cabeçalho do tipo Doc, nós iremos exportar o nome do tipo, mas não seus construtores. Isso irá prefinir que módulos que usarem o tip Doc criem e correspondam padrões com os valores Doc.

Ao invés de criar um Doc, um usuário do módulo `Prettify` irá chamar uma função que fornecemos. Aqui são as simples funções de construção. A medida que adicionamos a real definição, nós devemos substituir qualquer esboço que está no arquivo `Prettify.hs`. 

```haskell
-- file: src/Prettify.hs
empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)
```
O construtor `Line` representa uma quebra de linha. A função `line` cria quebra de linha _hard_, as quais sempre aparecem no output da nossa biblioteca. Às vezes nós queremos uma quebra de linha _soft_, as quais são usadas somente se uma linha é muito grande para caber em uma janela ou página. Nós introduziremos a função `softline` em breve.

```haskell
-- file: src/Prettify.hs
line :: Doc
line = Line
```
Quase tão simples quanto os construtores básicos é a função `(<>)`, que concatena dois valores Doc.

```haskell
-- file: src/Prettify.hs
(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y
```
Nós correspondemos o padrão `Empty` de forma que concatenar um valor Doc com  `Empty` a esquerda ou a direita não terá efeito. Isso nos preveni de acrescentar à árvore valores inúteis.
```
ghci> text "foo" <> text "bar"
Concat (Text "foo") (Text "bar")
ghci> text "foo" <> empty
Text "foo"
ghci> empty <> text "bar"
Text "bar"
```

![[Tip]](assets/tip.png)

Um momento matemático

Se colocarmos brevemente nossos chapéus matemáticos, nós podemos dizer que `Empty` é a identidade sobre a concatenação, pois nada acontece se concatenarmos um valor Doc com `Empty`. De forma semelhante, 0 é a identidade da adição, e o 1 a identidade da multiplicação. A perspectiva matemática tem consequências muito úteis, como veremos em vários lugares ao longo deste livro. 

Nossas funções `hcat` e `fsep` concatenam uma lista de valores Doc em um só. Na seção chamada ["Exercícios"](../cap04#exerc%C3%ADcios-1 "Exercícios"), nós mencionamos que podemos definir concatenação para listar usando `foldr`.

```haskell
-- file: src/Prettify.hs
concat :: [[a]] -> [a]
concat = foldr (++) []
```
Como `(<>)` é análogo a `(++)`, e `empty` a `[]`, nós veremos como poderiamos escrever  `hcat` e `fsep` como _folds_ também.

```haskell
-- file: src/Prettify.hs
hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty
```

A definição de `fsep` depende de várias outras funções.

```haskell
-- file: src/Prettify.hs
fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line
```
Isso deve uma pequena explicação. A função `softline` deve inserir uma nova linha se a linha atual ficar muito grande, ou um espaço, caso contrário. Como podemos fazer isso se nosso tipo Doc não conhece nada sobre a renderização? Nossa resposta é que toda vez que encontrarmos uma linha _soft_, nós mantemos duas representações alternativas do documento, usando o construtor `Union`.

```haskell
-- file: src/Prettify.hs
group :: Doc -> Doc
group x = flatten x \`Union\` x
```
Nossa função `flatten` substitui uma `Line` por um espaço, tranformando duas linhas em apenas uma.

```haskell
-- file: src/Prettify.hs
flatten :: Doc -> Doc
flatten (x \`Concat\` y) = flatten x \`Concat\` flatten y
flatten Line           = Char ' '
flatten (x \`Union\` _)  = flatten x
flatten other          = other
```
Note que sempre chamamos a função `flatten` no lado esquerdo de uma união: Este lado de cada união é sempre o mesmo tamanho (em caracteres), ou maior, que o lado direito. Nós iremos fazer uso dessa propriedade em nossa função de renderização abaixo.

### Renderização Compactada

Nós frequentemente precisamos usar a representação de um informação com menos caracteres quanto possível. Por exemplo, se estamos enviando um dado JSON por uma conexão de rede, não há sentido em deixar o JSON bonito: o software do outro lado não se preocupa se o dado está bonito ou não, e adicionar espaços em branco necessários para fazer o layout parecer bonito pode causar uma sobrecarga.

Por esses casos e porque é um pedaço de código simples de iniciar, nós forneceremos um modelo para uma função de compactar JSON.

```haskell
-- file: src/Prettify.hs
compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)
```
A função `compact` envolve seu argumento em uma lista, e aplica a função auxiliar `transform`. A função `transform` trata seu argumento como uma pilha de itens a serem processados, onde o primeiro elemento da lista é o topo da pilha.

A função `transform` usa o padrão `(d:ds)` para quebrar a pilha no seu início, `d`, e no restante, `ds`. Na nossa expressão `case`, os primeiros ramos fazem recursão em `ds`, consumindo um item da pilha por recursão. Os dois últimos ramos adicionam itens ao início do `ds`: o ramo `Concat` adiciona ambos os elementos a pilham enquanto o ramo `Union` ignora seu elemento da esquerda, o qual chamamos `flatten`, e adiciona seu elemento da direita a pilha. 

Agora, desenvolvemos o suficiente das nossas definições esqueléticas originais que podemos tentar usar nossa função `compact` no **ghci**. 

```
ghci> let value = renderJValue (JObject [("f", JNumber 1), ("q", JBool True)])
ghci> :type value
value :: Doc
ghci> putStrLn (compact value)
{"f": 1.0,
"q": true
}
```

Para entender melhor como o código funciona, vamos olhar um simples exemplo em mais detalhes.

```
ghci> char 'f' <> text "oo"
Concat (Char 'f') (Text "oo")
ghci> compact (char 'f' <> text "oo")
"foo"
```

Quando aplicamos `compact`, ele põe seu argumento em uma lista e aplica `transform`.

*   A função `transform` recebe um item da lista, o qual casa com o padrão `(d:ds)`. Então `d` é o valor `Concat (Char 'f') (Text "oo")`, e `ds` é a lista vazia, `[]`.
    
    Como o construtor `d` é `Concat`, o padrão `Concat` corresponde na expressão `case`. No lado direito, nós adicionamos `Char 'f'` e `Text "oo"` na pilha, e aplicamos `transform` recursivamente.
    
*   *   A função `transform` recebe uma lista de dois itens, novamente casando com o padrão `(d:ds)`. A variável `d` é vinculada a `Char 'f'`, e `ds` a `[Text "oo"]`.
        
        A expressão `case` casa no ramo `Char`. No lado direito, nós usamos `(:)` para construir uma lista onde o início é `'f'`, e o restante é o resultado da aplicação recursiva de `transform`. 
        
    *   *   A invocação recursiva recebe um item. A variável `d` e atribuído a `Text "oo"`, e `ds` para `[]`.
            
            A expressão `case` casa no  ramo `Text`. No lado direito, nós usamos `(++)` para concatenar `"oo"` com o resultado da aplicação recursiva de `transform`.  
            
        *   *   Na invocação final, `transform` é invocado com uma lista vazia, e retorna uma string vazia. 
                
            
        *   O resultado é `"oo" ++ ""`.
            
        
    *   O resultado é `'f' : "oo" ++ ""`.
        
    

### A verdadeira impressão agradável

Enquanto nossa função `compact` é útil para comunicadão maquina para maquina, seu resultado nem sempre é fácil para um ser humano seguir: há muito pouca informação em cada linha. Para  gerar saídas mais agradáveis, iremos escrever outra função, `pretty`. Comparado com `compact`, `pretty` necessita de um argumento a mais: a largura máxima de uma linha, em colunas. (Nós estamos assumindo que nosso tipo de letra tem tamanho fixo.)

```haskell
-- file: src/Prettify.hs
pretty :: Int -> Doc -> String
```
Para ser mais precisos, o paramento Int controla o comportamento de `pretty` quando encontra uma `softline`. Somente em uma `softline` a função terá a opção de continuar a na linha atual ou iniciar uma nova linha. Em outros lugares, nós devemos seguir rigorosamente as diretrizes estabelecidas pelo usuário do nosso `Prettify` módulo.

Aqui a parte central da nossa implementação

```haskell
-- file: src/Prettify.hs
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col
```
Nossa função auxiliar `best` recebe dois argumentos: o número de colunas usados na linha atual, e uma lista com o restante dos valores Doc para serem processados.

Nos casos simples,`best` atualiza a variável `col` de maneira direta, pois consome a entrada. Até o caso `Concat` é óbvio: Nós colocamos os dois componentes concatenados na pilha e não tocamos em `col`. 

O caso interessante envolve o construtor `Union`. Relembre que aplicamos  `flatten` ao elemento da esquerda, e não fizemos nada no da  direita. Relembre também que `flatten` substitui quebras de linhas para espaços. Portanto, nosso trabalho é ver qual dos dois layouts, o da esquerda ou o original, irá caber na nossa restrição de tamanho.

Para fazer isso, nós escrevemos uma pequena função auxiliar que determina se a linha unica ou o valor Doc irá caber no número de colunas dado.

```haskell
-- file: src/Prettify.hs
fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs
```

### Seguindo o fluxo de execução

Para entender como esse código funciona, vamos considerar um simples valor Doc.

```
ghci>  empty </> char 'a'
Concat (Union (Char ' ') Line) (Char 'a')
```

Vamos aplicar `pretty 2` nesse valor. Na primeira vez que aplicamos `best`, o valor de `col` é zero. E corresponde com o caso `Concat`, coloca o valor  `Union (Char ' ') Line` e `Char 'a'` na pilha, a se aplica recursivamente. Na aplicação recursiva ela casa com `Union (Char ' ') Line`.

Nesse ponto, vamos ignorar a forma usual de ordem de valoração em Haskell. Isso mantém simples nossa explicação do que está acontecendo, sem mudar o resultado final. Agora temos duas sub expressões, `best 0 [Char ' ', Char 'a']` e `best 0 [Line, Char 'a']`. A primeira é valorada para `" a"` e a segundo para `"\na"`. Então substituímos na expressão externa para obtermos `nicest 0 " a" "\na"`.  

Para entender o resultado de `nicest` aqui, nós faremos uma pequena substituição. O valor de `width` e `col` são 0 e 2, respectivamente, então `least` é 0, e `width - least` é 2. nós rapidamente valoramos ``2 `fits` " a"`` no **ghci**.

```
ghci> 2 `fits` " a"
True
```

Como é valorado para `True`, o resultado de `nicest` é `" a"`.

Se aplicarmos nosso função `pretty` ao mesmo JSON como antes, podemos ver que ela produz diferentes resultados dependendo da largura que definimos.

```
ghci> putStrLn (pretty 10 value)
{"f": 1.0,
"q": true
}
ghci> putStrLn (pretty 20 value)
{"f": 1.0, "q": true
}
ghci> putStrLn (pretty 30 value)
{"f": 1.0, "q": true }
```

#### Exercícios

Nossa atual biblioteca de impressão agradável é concisa, de modo que cabe dentro de nossas restrições de espaço, mas há várias melhoras úteis que podemos fazer.

**1.** Escreva a função `fill`, com a seguinte assinatura de tipos:
```
-- file: ch05/Prettify.hs
fill :: Int -> Doc -> Doc
```
Ela deve adicionar espaços em branco até que o número dado de colunas do documento esteja preenchido. Se o documento já é maior que o número de colunas determinado, então ele não deve adicionar espaços.

**2.**
Nosso `Prettify` não leva em indentação em conta. Quando abrimos parêntesis, chaves ou colchetes, todas as linha a seguir deverão ser indentadas para que fiquem alinhadas com seu carácter de abertura até que encontre um carácter de fechamento correspondente.
Adicione suporte para indentação, com uma quantidade controlável de indentação.
```
-- file: ch05/Prettify.hs
nest :: Int -> Doc -> Doc
```

### Criando um pacote 

A comunidade Haskell criou um conjunto padrão de ferramentas, chamado Cabal, que nos ajuda a construir, instalar e distribuir software. Cabal organiza softwares como pacotes. Um pacote contém uma biblioteca e, possívelmente, um conjunto de programas executáveis.

#### Escrevendo a descrição de um pacote

Para fazer algo com um pacote, Cabal precisa da sua descrição. Ela é contida  em um arquivo de texto cujo nome termina com o sufixo `.cabal`. Este  arquivo permanece no diretório com o nível mais alto do seu projeto. Ela tem um formato simples, que será descrito abaixo.

Um pacote Cabal deve ter um nome. Normalmente, o nome do pacote é o mesmo que o nome do arquivo `.cabal`. Frequentemente, o nome do diretório que contém o arquivo `.cabal` tem o mesmo nome que o pacote, por exemplo, `mypretty`. 

A descrição de um pacote inicia com uma série de propriedades globais, que se aplicam a todas as bibliotecas e executáveis no pacote.

```
Name:          mypretty
Version:       0.1

-- This is a comment.  It stretches to the end of the line.
```

Nomes de pacotes devem ser únicos. Se você criar e instalar um pacote que tem o mesmo nome que um pacote já instalado em seu sistema o GHC irá ficar bastante confuso.

As propriedades globais incluem uma quantidade substancial de informações destinadas aos leitores humanos, não ao próprio Cabal.

```
Synopsis:      My pretty printing library, with JSON support
Description:
  A simple pretty printing library that illustrates how to
  develop a Haskell library.
Author:        Real World Haskell
Maintainer:    nobody@realworldhaskell.org
```

Como o campo `Description` indica, um campo pode conter múltiplas linhas, desde que sejam indentadas.

Também inclusa nas propriedades globais são as informações de licença. A maiorias dos pacotes Haskell estão sob a licença BSD, que o Cabal chama de `BSD3`(Obviamente, você é livre para escolher qualquer licença que você ache apropriada.). O campo opcional `License-File` nos permite especificar o nome do arquivo que contém o texto exato do termo de licença do nosso pacote.

Os recursos suportados por sucessivas versões do Cabal evoluem com o tempo, então, é recomendado  indicar quais versões que esperamos ser compatível. Os recursos que estamos descrevendo são compatíveis com a versão 1.2 (ou maiores) do Cabal. 

```
Cabal-Version: >= 1.2
```

Para descrever uma biblioteca individual dentro de um pacote, nós escrevemos a seção `library`. O uso de indentação aqui é relevante: O conteúdo de uma seção deve ser indentado.

```
library
  Exposed-Modules: Prettify
                   PrettyJSON
                   SimpleJSON
  Build-Depends:   base >= 2.0
```

O campo `Exposed-Modules` contém uma lista de módulos que devem estar disponíveis aos usuários desse pacote. Um campo opcional, `Other-Modules`, contém uma lista de módulos internos. Estes são requisitos para que as bibliotecas funcionem, mas não serão visíveis aos usuários. 

O campo `Build-Modules` contém umas lista de pacotes (separados por vírgula) que nossas bibliotecas precisam para compilar. Para cada pacote nos podemos, opcionalmente, especificar uma série de versões que que fazem a biblioteca funcionar.

![[Tip]](assets/tip.png)

Entendendo a compilação de dependências

Nós não precisamos adivinhar ou fazer qualquer pesquisa para estabelecer qual pacotes precisamos. Se você tentar compilar nosso pacote sem o campo `Build-Depends`, a compilação irá falhar com uma mensagem de error útil. Aqui um exemplo onde retiramos a dependência do pacote `base`.

```
$ runghc Setup build
Preprocessing library mypretty-0.1...
Building mypretty-0.1...

PrettyJSON.hs:8:7:
    Could not find module `Data.Bits':
      it is a member of package base, which is hidden
```
A mensagem de erro deixa claro que precisamos do pacote `base`, mesmo que `base` já esteja instalada. Forçar-nos a explicitar todos os pacotes que dependemos tem um benefício prático: uma ferramenta de linha de comando chamada `cabal-install` irá automaticamente baixar, compilar e instalar o pacote e todos os pacotes que ele depende.

#### Gerenciador de pacotes GHC

GHC inclui um simples gerenciador de pacotes que monitora quais pacotes são instalados e quais são suas versões . Uma ferramenta de linha de comando chamada **ghc-pkg** nos permite trabalhar com seus bancos de dados de pacotes.

Nós falamos _banco de dados_  porque GHC distingue pacotes do sistema, que são disponíveis para todos os usuários, e pacotes por usuário, que são disponíveis apenas para o usuário atual. Os pacotes por usuário nos permitem evitar a necessidade de privilégios administrativos para instalar novos pacotes.

O comando **ghc-pkg** providencia sub comandos para performar diferentes tarefas. Na maioria do tempo, nós precisaremos de apenas duas delas. O comando *ghc-pkg list* nos permite ver quais pacotes estão instalados. Quando queremos desinstalar um pacote, *ghv-pkg unregister* fala ao GHC que não usaremos um pacote nunca mais. (Veremos que teremos que remover manualmente os arquivos instalados.)

#### Configurando, compilando e instalando

Além do arquivo `.cabal`, um pacote deve conter um arquivo _setup_. Isto permite  que o processo de compilação do Cabal seja altamente personalizado, se um pacote precisar. O simples arquivo _setup_ é assim:

```
-- file: ch05/Setup.hs
#!/usr/bin/env runhaskell
import Distribution.Simple
main = defaultMain
```

Agora salve esse arquivo com o nome `Setup.hs`.

Uma vez que temos os arquivos `.cabal` e `Setup.hs` escritos, nos resta apenas três passos.

Para instruir o Cabal como compilar e onde instalar um pacote, nos executamos um simples comando:

```
$ runghc Setup configure
```

Isto garante que os pacotes que precisamos estão disponíveis e guarda as configurações para ser usadas depois por outros comandos do Cabal.

Se não fornecemos nem um argumento ao `configure`, Cabal irá instalar o pacote no banco de dados de pacotes do sistema. Para instalar no nosso diretório home e no nosso banco de dados pessoal, nós devemos fornecer um pouco mais de informações.

```
$ runghc Setup configure --prefix=$HOME --user
```

Seguindo o passo de configuração, nós compilamos o pacote.

```
$ runghc Setup build
```

Se isso obter sucesso, nós podemos instalar o pacote. Não precisamos identificar onde instalá-lo: Cabal irá utilizar as configurações que fornecemos anteriormente. Isso irá instalar em nosso diretório a atualizar o banco de dados do GHC por usuário.

```
$ runghc Setup install
```

### Dicas práticas e leitura adicional


GHC já inclui uma biblioteca de impressão agradável, `Text.PrettyPrint.HughesPJ`. Ela fornece as mesmas API básicas como nosso exemplo, porém muito mais rica e mais funções úteis. Nós recomendamos usá-la ao invés de escrever a sua própria. 

O desenvolvimento de `HughesPJ` foi introduzido em  \[[Hughes95](bibliography.md#bib.hughes95 "[Hughes95]")\]. Essa biblioteca foi subsequentemente melhorada por Simon Peyton Jones, daí o nome. O artigo de Hughes é longo mais vale a pena ler por sua discussão de como projetar uma biblioteca em Haskell.

Nesse capítulo, nossa biblioteca de impressão agradável é baseada em um simples sistema descrito por Philip Wadler em \[[Wadler98](bibliography.#bib.wadler98 "[Wadler98]")\]. Sua biblioteca foi estendida por Daan Leijen; essa versão  está disponível para download em Hackage como `wl-pprint`. Se você usa a ferramenta de linha de comando **cabal**, você pode baixar, compilar, e instalar em um passo: **cabal install wl-pprint**.

  

* * *

\[[10](#id598725)\] Memory aid: `-o` stands for “output” or “object file”.

\[[11](#id602026)\] The “3” in `BSD3` refers to the number of clauses in the license. An older version of the BSD license contained 4 clauses, but it is no longer used.

![](assets/rss.png) Want to stay up to date? Subscribe to the comment feed for [this chapter](/feeds/comments/), or the [entire book](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen. This work is licensed under a [Creative Commons Attribution-Noncommercial 3.0 License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul Davey](mailto:mattahan@gmail.com) aka [Mattahan](http://mattahan.deviantart.com/).

[Prev](functional-programming.md)

[Next](using-typeclasses.md)

Chapter 4. Functional programming

[Home](index.md)

Chapter 6. Using Typeclasses

_uacct = "UA-1805907-3"; urchinTracker();
