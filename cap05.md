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


### Printing JSON data


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

Para termos certeza que `Prettify` atende às necessidades práticas, iremos escrever um novo renderizador de JSON que use a API `Prettify` `Prettify`. Depois que estiver pronto, nós voltaremos e entramos em detalhes do `Prettify` módulo. 

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

#### Impressão bonita de uma string


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
    ghci> 
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

### Arrays and objects, and the module header


Compared to strings, pretty printing arrays and objects is a snap. We already know that the two are visually similar: each starts with an opening character, followed by a series of values separated with commas, followed by a closing character. Let's write a function that captures the common structure of arrays and objects.

```haskell
-- file: src/PrettyJSON.hs
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item
```
We'll start by interpreting this function's type. It takes an opening and closing character, then a function that knows how to pretty print a value of some unknown type `a`, followed by a list of values of type `a`, and it returns a value of type Doc.

Notice that although our type signature mentions four parameters, we have only listed three in the definition of the function. We are simply following the same rule that lets us simplify a definiton like `myLength xs = length xs` to `myLength = length`.

We have already written `enclose`, which wraps a Doc value in opening and closing characters. The `fsep` function will live in our `Prettify` module. It combines a list of Doc values into one, possibly wrapping lines if the output will not fit on a single line.

```haskell
-- file: src/Prettify.hs
fsep :: [Doc] -> Doc
fsep xs = undefined
```
By now, you should be able to define your own stubs in `Prettify.hs`, by following the examples we have supplied. We will not explicitly define any more stubs.

The `punctuate` function will also live in our `Prettify` module, and we can define it in terms of functions for which we've already written stubs.

```haskell
-- file: src/Prettify.hs
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds  
```

With this definition of `series`, pretty printing an array is entirely straightforward. We add this equation to the end of the block we've already written for our `renderJValue` function.

```haskell
-- file: src/PrettyJSON.hs
renderJValue (JArray ary) = series '\[' '\]' renderJValue ary
```
To pretty print an object, we need to do only a little more work: for each element, we have both a name and a value to deal with.

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


### Writing a module header

Now that we have written the bulk of our `PrettyJSON.hs` file, we must go back to the top and add a module declaration.

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
We export just one name from this module: `renderJValue`, our JSON rendering function. The other definitions in the module exist purely to support `renderJValue`, so there's no reason to make them visible to other modules.

Regarding imports, the `Numeric` and `Data.Bits` modules are distributed with GHC. We've already written the `SimpleJSON` module, and filled our `Prettify` module with skeletal definitions. Notice that there's no difference in the way we import standard modules from those we've written ourselves.

With each `import` directive, we explicitly list each of the names we want to bring into our module's namespace. This is not required: if we omit the list of names, all of the names exported from a module will be available to us. However, it's generally a good idea to write an explicit import list.

*   An explicit list makes it clear which names we're importing from where. This will make it easier for a reader to look up documentation if they encounter an unfamiliar function.
    
*   Occasionally, a library maintainer will remove or rename a function. If a function disappears from a third party module that we use, any resulting compilation error is likely to happen long after we've written the module. The explicit list of imported names can act as a reminder to ourselves of where we had been importing the missing name from, which will help us to pinpoint the problem more quickly.
    
*   It can also occur that someone will add a name to a module that is identical to a name already in our own code. If we don't use an explicit import list, we'll end up with the same name in our module twice. If we use that name, GHC will report an error due to the ambiguity. An explicit list lets us avoid the possibility of accidentally importing an unexpected new name.
    

This idea of using explicit imports is a guideline that usually makes sense, not a hard-and-fast rule. Occasionally, we'll need so many names from a module that listing each one becomes messy. In other cases, a module might be so widely used that a moderately experienced Haskell programmer will probably know which names come from that module.

### Fleshing out the pretty printing library


In our `Prettify` module, we represent our Doc type as an algebraic data type.

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
Observe that the Doc type is actually a tree. The `Concat` and `Union` constructors create an internal node from two other Doc values, while the `Empty` and other simple constructors build leaves.

In the header of our module, we will export the name of the type, but not any of its constructors: this will prevent modules that use the Doc type from creating and pattern matching against Doc values.

Instead, to create a Doc, a user of the `Prettify` module will call a function that we provide. Here are the simple construction functions. As we add real definitions, we must replace any stubbed versions already in the `Prettify.hs` source file.

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
The `Line` constructor represents a line break. The `line` function creates _hard_ line breaks, which always appear in the pretty printer's output. Sometimes we'll want a _soft_ line break, which is only used if a line is too wide to fit in a window or page. We'll introduce a `softline` function shortly.

```haskell
-- file: src/Prettify.hs
line :: Doc
line = Line
```
Almost as simple as the basic constructors is the `(<>)` function, which concatenates two Doc values.

```haskell
-- file: src/Prettify.hs
(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y
```
We pattern match against `Empty` so that concatenating a Doc value with `Empty` on the left or right will have no effect. This keeps us from bloating the tree with useless values.
```
    ghci> 
```
![[Tip]](/support/figs/tip.png)

A mathematical moment

If we briefly put on our mathematical hats, we can say that `Empty` is the identity under concatenation, since nothing happens if we concatenate a Doc value with `Empty`. In a similar vein, 0 is the identity for adding numbers, and 1 is the identity for multiplying them. Taking the mathematical perspective has useful practical consequences, as we will see in a number of places throughout this book.

Our `hcat` and `fsep` functions concatenate a list of Doc values into one. In [the section called “Exercises”](functional-programming.html#fp.fold.exercises "Exercises"), we mentioned that we could define concatenation for lists using `foldr`.

```haskell
-- file: src/Prettify.hs
concat :: [[a]] -> [a]
concat = foldr (++) []
```
Since `(<>)` is analogous to `(++)`, and `empty` to `[]`, we can see how we might write `hcat` and `fsep` as folds, too.

```haskell
-- file: src/Prettify.hs
hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty
```

The definition of `fsep` depends on several other functions.

```haskell
-- file: src/Prettify.hs
fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line
```
These take a little explaining. The `softline` function should insert a newline if the current line has become too wide, or a space otherwise. How can we do this if our Doc type doesn't contain any information about rendering? Our answer is that every time we encounter a soft newline, we maintain _two_ alternative representations of the document, using the `Union` constructor.

```haskell
-- file: src/Prettify.hs
group :: Doc -> Doc
group x = flatten x \`Union\` x
```
Our `flatten` function replaces a `Line` with a space, turning two lines into one longer line.

```haskell
-- file: src/Prettify.hs
flatten :: Doc -> Doc
flatten (x \`Concat\` y) = flatten x \`Concat\` flatten y
flatten Line           = Char ' '
flatten (x \`Union\` _)  = flatten x
flatten other          = other
```
Notice that we always call `flatten` on the left element of a `Union`: the left of each `Union` is always the same width (in characters) as, or wider than, the right. We'll be making use of this property in our rendering functions below.

### Compact rendering

We frequently need to use a representation for a piece of data that contains as few characters as possible. For example, if we're sending JSON data over a network connection, there's no sense in laying it out nicely: the software on the far end won't care whether the data is pretty or not, and the added white space needed to make the layout look good would add a lot of overhead.

For these cases, and because it's a simple piece of code to start with, we provide a bare-bones compact rendering function.

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
The `compact` function wraps its argument in a list, and applies the `transform` helper function to it. The `transform` function treats its argument as a stack of items to process, where the first element of the list is the top of the stack.

The `transform` function's `(d:ds)` pattern breaks the stack into its head, `d`, and the remainder, `ds`. In our `case` expression, the first several branches recurse on `ds`, consuming one item from the stack for each recursive application. The last two branches add items in front of `ds`: the `Concat` branch adds both elements to the stack, while the `Union` branch ignores its left element, on which we called `flatten`, and adds its right element to the stack.

We have now fleshed out enough of our original skeletal definitions that we can try out our `compact` function in **ghci**.

    ghci> 

To better understand how the code works, let's look at a simpler example in more detail.

    ghci> 

When we apply `compact`, it turns its argument into a list and applies `transform`.

*   The `transform` function receives a one-item list, which matches the `(d:ds)` pattern. Thus `d` is the value `Concat (Char 'f') (Text "oo")`, and `ds` is the empty list, `[]`.
    
    Since `d`'s constructor is `Concat`, the `Concat` pattern matches in the `case` expression. On the right hand side, we add `Char 'f'` and `Text "oo"` to the stack, and apply `transform`recursively.
    
*   *   The `transform` function receives a two-item list, again matching the `(d:ds)` pattern. The variable `d` is bound to `Char 'f'`, and `ds` to `[Text "oo"]`.
        
        The `case` expression matches in the `Char` branch. On the right hand side, we use `(:)` to construct a list whose head is `'f'`, and whose body is the result of a recursive application of `transform`.
        
    *   *   The recursive invocation receives a one-item list. The variable `d` is bound to `Text "oo"`, and `ds` to `[]`.
            
            The `case` expression matches in the `Text` branch. On the right hand side, we use `(++)` to concatenate `"oo"` with the result of a recursive application of `transform`.
            
        *   *   In the final invocation, `transform` is invoked with an empty list, and returns an empty string.
                
            
        *   The result is `"oo" ++ ""`.
            
        
    *   The result is `'f' : "oo" ++ ""`.
        
    

### True pretty printing

While our `compact` function is useful for machine-to-machine communication, its result is not always easy for a human to follow: there's very little information on each line. To generate more readable output, we'll write another function, `pretty`. Compared to `compact`, `pretty` takes one extra argument: the maximum width of a line, in columns. (We're assuming that our typeface is of fixed width.)

```haskell
-- file: src/Prettify.hs
pretty :: Int -> Doc -> String
```
To be more precise, this Int parameter controls the behaviour of `pretty` when it encounters a `softline`. Only at a `softline` does `pretty` have the option of either continuing the current line or beginning a new line. Elsewhere, we must strictly follow the directives set out by the person using our pretty printing functions.

Here's the core of our implementation

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
Our `best` helper function takes two arguments: the number of columns emitted so far on the current line, and the list of remaining Doc values to process.

In the simple cases, `best` updates the `col` variable in straightforward ways as it consumes the input. Even the `Concat` case is obvious: we push the two concatenated components onto our stack/list, and don't touch `col`.

The interesting case involves the `Union` constructor. Recall that we applied `flatten` to the left element, and did nothing to the right. Also, remember that `flatten` replaces newlines with spaces. Therefore, our job is to see which (if either) of the two layouts, the `flatten`ed one or the original, will fit into our `width` restriction.

To do this, we write a small helper that determines whether a single line of a rendered Doc value will fit into a given number of columns.

```haskell
-- file: src/Prettify.hs
fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs
```

### Following the pretty printer

In order to understand how this code works, let's first consider a simple Doc value.

*Main Prettify PrettyJSON PutJSON SimpleJSON>  empty </> char 'a'
Concat (Union (Char ' ') Line) (Char 'a')

We'll apply `pretty 2` on this value. When we first apply `best`, the value of `col` is zero. It matches the `Concat` case, pushes the values `Union (Char ' ') Line` and `Char 'a'` onto the stack, and applies itself recursively. In the recursive application, it matches on `Union (Char ' ') Line`.

At this point, we're going to ignore Haskell's usual order of evaluation. This keeps our explanation of what's going on simple, without changing the end result. We now have two subexpressions, `best 0 [Char ' ', Char 'a']` and `best 0 [Line, Char 'a']`. The first evaluates to `" a"`, and the second to `"\na"`. We then substitute these into the outer expression to give `nicest 0 " a" "\na"`.

To figure out what the result of `nicest` is here, we do a little substitution. The values of `width` and `col` are 0 and 2, respectively, so `least` is 0, and `width - least` is 2. We quickly evaluate ``2 `fits` " a"`` in **ghci**.

*Main Prettify PrettyJSON PutJSON SimpleJSON> 2 `fits` " a"
True

Since this evaluates to `True`, the result of `nicest` here is `" a"`.

If we apply our `pretty` function to the same JSON data as earlier, we can see that it produces different output depending on the width that we give it.

```haskell
-- file: app/Main.hs
module Main (main) where

import SimpleJSON
import Prettify
import PrettyJSON


value = renderJValue $ JObject [("f", JNumber 1), ("q", JBool True)]
main :: IO ()
main = do
    putStrLn (pretty 10 value)
    putStrLn (pretty 20 value)
    putStrLn (pretty 30 value)
```
    
Executando
```
$stack run
{"f": 1.0,
"q": true
}
{"f": 1.0, "q": true
}
{"f": 1.0, "q": true }
```


### Practical pointers and further reading


GHC already bundles a pretty printing library, `Text.PrettyPrint.HughesPJ`. It provides the same basic API as our example, but a much richer and more useful set of pretty printing functions. We recommend using it, rather than writing your own.

The design of the `HughesPJ` pretty printer was introduced by John Hughes in \[[Hughes95](bibliography.html#bib.hughes95 "[Hughes95]")\]. The library was subsequently improved by Simon Peyton Jones, hence the name. Hughes's paper is long, but well worth reading for his discussion of how to design a library in Haskell.

In this chapter, our pretty printing library is based on a simpler system described by Philip Wadler in \[[Wadler98](bibliography.html#bib.wadler98 "[Wadler98]")\]. His library was extended by Daan Leijen; this version is available for download from Hackage as `wl-pprint`. If you use the **cabal** command line tool, you can download, build, and install it in one step with **cabal install wl-pprint**.

  

* * *

\[[10](#id598725)\] Memory aid: `-o` stands for “output” or “object file”.

\[[11](#id602026)\] The “3” in `BSD3` refers to the number of clauses in the license. An older version of the BSD license contained 4 clauses, but it is no longer used.

![](/support/figs/rss.png) Want to stay up to date? Subscribe to the comment feed for [this chapter](/feeds/comments/), or the [entire book](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen. This work is licensed under a [Creative Commons Attribution-Noncommercial 3.0 License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul Davey](mailto:mattahan@gmail.com) aka [Mattahan](http://mattahan.deviantart.com/).

[Prev](functional-programming.html)

[Next](using-typeclasses.html)

Chapter 4. Functional programming

[Home](index.html)

Chapter 6. Using Typeclasses

_uacct = "UA-1805907-3"; urchinTracker();
