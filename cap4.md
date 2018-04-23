---
layout: page
---
Uma tradução não oficial do livro Real World Haskell 
de Bryan O'Sullivan, Don Stewart, and John Goerzen

-------------------------------------------------------


## Capítulo 4. Programação funcional


### Pensando no Haskell

Nossa aprendizagem inicial em Haskell possui dois aspectos distintos. A primeira é a mudança de mentalidade da programação imperativa para funcional: temos de substituir nossos hábitos de programação em outras linguagens. Fazemos isso não porque as técnicas imperativas são ruins, mas porque, em uma linguagem funcional outras técnicas funcionam melhor. 

O nosso segundo desafio é aprender a nossa maneira de lidar com as bibliotecas basicas do Haskell. Como em qualquer linguagem, bibliotecas  funcionam como alavanca, habilitando-nos a multiplicar a nossa solução de problemas. Bibliotecas do Haskell tendem a operar em um nível maior de abstração do que aqueles em muitas outras linguagens. Precisaremos trabalhar um pouco mais para aprender usar as bibliotecas, mas na troca elas oferecem uma muito poder. 

Neste capítulo, vamos introduzir uma série de técnicas de programação funcional. Nós vamos recorrer a exemplos de linguagens imperativas, destacando a mudança no pensamento que vamos precisar fazer. A medida que avançarmos, iremos cobrindo alguns dos fundamentos das bibliotecas padrão do Haskell. Nós também iremos cobrir um pouco mais os recursos da linguagem.

### Um simple framework de linha de comando 

Na maioria deste capítulo, iremos trabalhar com códigos sem interacção com o mundo exterior. Contudo, para manter o foco em códigos aplicados a problemas do mundo real, vamos começar desenvolvendo uma passagem entre o nosso código “puro” e o mundo lá fora. O nosso framework simplesmente lê o conteúdo de um arquivo, aplica uma função para o arquivo e escreve o resultado em outro arquivo. 

```haskell
-- file: ch04/InteractWith.hs
-- Save this in a source file, e.g. Interact.hs

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = id
```

Isto é tudo que nós necessitamos para escrever um simples, mas completo, programa para processamento de arquivos. Esse é um programa completo. Nós podemos compilá-lo para um executável chamado `InteractWith`como se segue. 

    $ ghc --make InteractWith
    [1 of 1] Compiling Main             ( InteractWith.hs, InteractWith.o )
    Linking InteractWith ...


Se executar este programa a partir de um shell ou prompt de comando, que aceita dois nomes de arquivos: o nome da arquivo de leitura, o nome e de um arquivo para escrita. 

    $ ./InteractWith
    error: exactly two arguments needed
    $ ./InteractWith hello-in.txt hello-out.txt
    $ cat hello-in.txt
    hello world
    $ cat hello-out.txt
    hello world


Algumas das notação nesse arquivo fonte é nova. A palavra-chave `do` introduz um bloco de ações que podem provocar efeitos no mundo real, tais como a leitura ou a escrita em um arquivo. O operador `<-` pode ser compreendido como a uma atribuição dentro um bloco `do`. Esta explicação é o bastante para começarmos. Falaremos em muito mais detalhes sobre os detalhes da notação, e de I/O em geral, em [Capítulo 7, _I/O_](). 

Quando se deseja testar uma função que não pode falar com o mundo lá fora, nos simplesmente substituimos o nome `id` no código acima com o nome da função que queremos testar. Independente do que a nossa função faz, ela precisa ter o tipo de `String->String`: em outras palavras, ela deve aceitar uma string e retornar uma string. 

### Warming up: Separação das linhas de texto portavel


Haskell provê a função `lines`, que divide uma string de texto em linhas. Ela retorna uma lista das cadeias de caracteres com terminação de linha omitida. 

    ghci> :type lines
    lines :: String -> [String]
    ghci> lines "line 1\nline 2"
    ["line 1","line 2"]
    ghci> lines "foo\n\nbar\n"
    ["foo","","bar"]

Embora `lines`pareça útil, ela assume que estamos lendo um arquivo em "modo texto" para funcionar. Modo texto é uma característica comum a muitas linguagens: proporciona um comportamento especial quando lêem e escrevem arquivos no Windows. Quando se lê um arquivo em modo de texto, o arquivo de biblioteca I/O traduz o fim de linha `"\r\n"`(retorno do carro seguido por nova linha) para `"\n"`(apenas nova linha ), e faz o inverso ao escrever um arquivo. Em sistemas Unix, o modo de texto não exerce qualquer traduçao. Como resultado desta diferença, se ler um arquivo em uma plataforma que estava escrito em outra, o final de linha devem se tornar uma bagunça. (Ambos `readFile`e `writeFile`operam em modo texto). 

    ghci> lines "a\r\nb"
    ["a\r","b"]

A função `lines` só divide a partir do caracter "nova linha", deixando o caracter "retorno de carros" pendente nas extremidades das linhas. Se ler um arquivo de texto gerado no Windows em um sistema Unix nós vamos manter o caracter de "retorno de carro" no final da cada linha. 

Nós temos confortavelmente usado “universal newline” do Python por anos: que trata isso de forma transparente as convenções de quebra de linha no Unix e Windows para nós. Gostaríamos de oferecer algo similar em Haskell. 

Dado que estamos ainda aprendendo ler códigos em  Haskell, vamos discutir nossa aplicação em Haskell com bastante detalhe. 

```haskell
-- file: ch04/SplitLines.hs
splitLines :: String -> [String]
```
A assinatura do tipo de nossa função indica que ela aceita uma única String, o conteúdo de um arquivo com alguma convençao de "quebra de linha" desconhecida. Então, ela retorna uma lista de String, que representa cada linha do arquivo. 

```haskell
-- file: ch04/SplitLines.hs
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'
```
Antes de nos aprofundarmos em detalhes, note primeiro como o código está organizado. Nós apresentamos as partes importantes de código em primeiro lugar, mantendo a definição de `isLineTerminator` para o final. Dado que demos a função auxiliar um nome legível, podemos inferir o que ela faz memso antes de termos lido, facilitando a leitura do código. 

O Prelude define uma função chamada `break`que podemos usar para particionar um de lista em duas partes. Ela toma uma função como seu primeiro parâmetro. Essa função deverá examinar elementos de lista, e retorna um valor Bool para indicar se deseja interromper a lista nesse momento. A função `break`retorna um par, que consiste na sublista consumida antes do predicado ter retornado `True`(o _prefixo_), eo resto da lista (o _sufixo_). 

    ghci> break odd [2,4,5,6,8]
    ([2,4],[5,6,8])
    ghci> :module +Data.Char
    ghci> break isUpper "isUpper"
    ("is","Upper")

Uma vez que só precisamos corresponder a um único "retorno de carro" ou "quebra de linha" por vez, analisar um elemento da lista por vés é o suficiente para as nossas necessidades. 

A primeira equação de `splitLines`indica que, se coincidir com uma seqüência vazia, não precismaos fazer nada, apenas retornarmos uma sequencia também vazia. 

Na segunda equação, aplicamos primeiro o `break` na nossa string de entrada. O prefixo é a substring antes de um terminador de linha, e sufixo é o restante da string. O sufixo incluirá o terminador de linha, caso esteja presente.

A expressão `pre :` indica-nos que devemos adicionar o valor `pre` na frente da lista das linhas. Em seguida, usamos uma expressão  `case` para inspecionar os sufixos, então podemos decidir o que fazer a partir do seu padão (casamento de padrões). O resultado da expressão `case`será utilizada como segundo argumento do construtor da lista `(:)`. 

O primeiro padrão corresponde a uma seqüência que começa com um "retorno de carro", seguido por uma "quebra de linha". A variável `rest`está amarrado ao restante da cadeia. Os outros padrões são parecidos, então elas devem ser faceis de acompanhar. 

Uma descrição de uma função Haskell não são necessariamente fáceis de seguir. Podemos obter uma melhor compreensão executando passo a passo no **ghci**, e observar o comportamento da função em circunstâncias diferentes. 

Começamos particionando por uma seqüência que não contém qualquer separador de linhas. 

    ghci> splitLines "foo"
    ["foo"]


Aqui a nossa aplicação da `break`nunca encontra um terminador de linha, deste modo o sufixo retorna vazio. 

    ghci> break isLineTerminator "foo"
    ("foo","")


A expressão `case`em `splitLines`como tal devem casar no quarto padrão. Agora, que tal considerar um caso um pouco mais interessante? 

    ghci> splitLines "foo\r\nbar"
    ["foo","bar"]

Nossa primeira aplicação de `break`nos oferece um sufixo não vazio. 

    ghci> break isLineTerminator "foo\r\nbar"
    ("foo","\r\nbar")


Devido o sufixo começa com um "retorno de carro", seguido de uma "quebra de linha", que irá casar como o primeira padrão da expressão `case`. Isto dá-nos `prefixo` amarrado a `"foo"`, e `sufixo`amarrado a `"bar"`. Nós então aplicamos `splitLines`recursivamente, desta vez no `"bar"`sozinho. 

    ghci> splitLines "bar"
    ["bar"]

O resultado é que vamos construir a lista cuja cabeça é `"foo"`e cuja cauda é `["bar"]`. 

    ghci> "foo" : ["bar"]
    ["foo","bar"]



Este tipo de experimentos com **ghci** é uma maneira útil de entender e depurar o comportamento de um código. Tem um benefício ainda mais importante que é quase acidental por natureza. Dado que pode ser complicado testar códigos complicados no **ghci**, então tenderemos a escrever funções menores. Isso pode ajudar ainda mais a legibilidade do nosso código

Este estilo de criar e de reutilização de partes pequenas, e poderosas de código é uma parte fundamental da programação funcional. 

#### Um programa de conversão de fim de linha

Vamos conectar a nossa função `splitLines`ao framework que escrevemos anteriormente. Faça um cópia do arquivo de fonte `Interact.hs`; vamos chamar o arquivo novo de `FixLines.hs.hs`. Adicione a função `splitLines`para o novo arquivo fonte. Desde que a nossa função precisa produzir um único String, temos que concatenar a lista de linhas de volta. O Prelude fornece uma função `unlines`que concatena a lista das strings, acrescentando uma nova linha para o final de cada uma. 

```haskell
-- file: ch04/SplitLines.hs
fixLines :: String -> String
fixLines input = unlines (splitLines input)
```

Se substituirmos a função `id`com `fixLines`, podemos compilar um executável que irá converter um arquivo de texto para o sistema nativo de termino de linha do nosso sistema. 

    $ ghc --make FixLines
    [1 of 1] Compiling Main             ( FixLines.hs, FixLines.o )
    Linking FixLines ...

Se você estiver em um sistema Windows, localize e baixe um arquivo de texto que foi criado em um sistema Unix (por exemplo, gpl-3.0.txt). Abra-o no editor de texto padrão do Bloco de Notas. Todas as linhas devem ser executadas juntas, tornando o arquivo quase ilegível. Processe o arquivo usando o comando **FixLines** que você acabou de criar e abra o arquivo de saída no Bloco de Notas. Os finais de linha agora devem ser corrigidos.

Em sistemas do tipo Unix, os pagers e editores padrão ocultam os términos de linha do Windows. Isso torna mais difícil verificar se os **FixLines** estão realmente eliminando-os. Aqui estão alguns comandos que devem ajudar

    $ file gpl-3.0.txt
    gpl-3.0.txt: ASCII English text
    $ unix2dos gpl-3.0.txt
    unix2dos: converting file gpl-3.0.txt to DOS format ...
    $ file gpl-3.0.txt
    gpl-3.0.txt: ASCII English text, with CRLF line terminator



### Funções infixas

Normalmente, quando definimos ou aplicamos uma função em Haskell, escrevemos o nome da função, seguido de seus argumentos. Essa notação é chamada de prefixo, porque o nome da função vem antes de seus argumentos.

Se uma função ou construtor usa dois ou mais argumentos, temos a opção de usá-lo no infix, onde o colocamos entre o primeiro e o segundo argumentos. Isso nos permite usar funções como operadores infixos.

Para definir ou aplicar um construtor de função ou valor usando a notação infixada, colocamos seu nome em caracteres backtick (às vezes conhecidos como backquotes). Aqui estão as definições simples de infix de uma função e um tipo.

```haskell
-- file: ch04/Plus.hs
a `plus` b = a + b

data a `Pair` b = a `Pair` b
                  deriving (Show)

-- we can use the constructor either prefix or infix
foo = Pair 1 2
bar = True `Pair` "quux"
```

Dado que a notação infixa é meramente uma conveniência sintática, não muda o comportamento da função. 

    ghci> 1 `plus` 2
    3
    ghci> plus 1 2
    3
    ghci> True `Pair` "something"
    True `Pair` "something"
    ghci> Pair True "something"
    True `Pair` "something"

A notação Infix pode frequentemente pode ajudar legibilidade. Por exemplo, Prelude define uma função, `elem`, que indicam se há um valor está presente em uma lista. Se usarmos `elem` na notaço prefixada, é bastante fácil de ler. 

    ghci> elem 'a' "camogie"
    True

Se mudarmos para a notação infixada, o código fica ainda mais fácil de entender. Agora está mais claro que estamos verificando se o valor à esquerda está presente na lista à direita

    ghci> 3 `elem` [1,2,4,8]
    False

Nós vemos uma melhoria mais pronunciada com algumas funções úteis do módulo Data.List. A função isPrefixOf indica se uma lista corresponde ao início de outra.

    ghci> :module +Data.List
    ghci> "foo" `isPrefixOf` "foobar"
    True 

As funções `isInfixOf`e `isSuffixOf`correspondem em qualquer lugar em uma lista e em seu final, respectivamente. 

    ghci> "needle" `isInfixOf` "haystack full of needle thingies"
    True
    ghci> "end" `isSuffixOf` "the end"
    True

Não existe uma regra rígida que determine quando você deve usar a notação infix versus prefix, embora a notação de prefixo seja muito mais comum. É melhor escolher o que tornar seu código mais legível em uma situação específica. 

>![[Note]](support/figs/note.png)**Cuidados com anotação familia em uma linguagem desconhecida**

>Algumas outras linguagens de programação usam backticks, mas apesar das semelhanças visuais, o propósito dos backticks em Haskell não remotamente lembra seu significado em, por exemplo, scripts shell Perl, Python ou Unix. Sem comentários

>A única coisa legal que podemos fazer com os backticks em Haskell é envolvê-los em torno do nome de uma função. Não podemos, por exemplo, usá-los para incluir uma expressão complexa cujo valor é uma função. Pode ser conveniente se pudéssemos, mas não é assim que a linguagem é hoje 

### Trabalhando com as listas


Como o pão e a manteiga da programação funcional, as listas merecem alguma atenção séria. A biblioteca padrão Prelude define dezenas de funções para lidar com listas. Muitas dessas ferramentas serão indispensáveis, por isso é importante aprendê-las desde o início. 

Para melhor ou pior, esta seção vai parecer uma enxurrada de funções para listas. Por que apresentar tantas funções ao mesmo tempo? Essas funções são fáceis de aprender e absolutamente onipresentes. Se não tivermos essa caixa de ferramentas na ponta dos dedos, acabaremos perdendo tempo reinventando funções simples que já estão presentes nas bibliotecas padrão. Então, tenha paciência conosco enquanto passamos pela lista; o esforço que você vai economizar será enorme.

O módulo Data.List é o local lógico e “real” de todas as funções padrão de lista. O Prelude meramente re-exporta um grande subconjunto das funções exportadas pelo Data.List. Várias funções úteis em Data.List não são exportadas novamente pelo prelúdio padrão. À medida que percorremos as funções de lista nas seções a seguir, mencionaremos explicitamente aquelas que estão apenas em Data.List.

    ghci> :module +Data.List

Como nenhuma dessas funções são complexas ou levam mais de três linhas de Haskell para escrever, seremos breves em nossas descrições de cada uma. De fato, um exercício de aprendizado rápido e útil é escrever uma definição de cada função depois de ler sobre ela.

#### Manipulação de listas básica

A função `length` nos informa quantos elementos estão em uma lista.

    ghci> :type length
    length :: [a] -> Int
    ghci> length []
    0
    ghci> length [1,2,3]
    3
    ghci> length "strings are lists, too"
    22


Se você precisa determinar se uma lista está vazia, use a função `null`.

    ghci> :type null
    null :: [a] -> Bool
    ghci> null []
    True
    ghci> null "plugh"
    False

Para acessar o primeiro elemento de uma lista, usamos a função `head`.

    ghci> :type head
    head :: [a] -> a
    ghci> head [1,2,3]
    1

O inverso, `tail`, retorna tudo, exceto a cabeça de uma lista.

    ghci> :type tail
    tail :: [a] -> [a]
    ghci> tail "foo"
    "oo" 

Outra função, `last`, retorna o último elemento de uma lista.

    ghci> :type last
    last :: [a] -> a
    ghci> last "bar"
    'r'

O inverso da `last` é `init`, que retorna uma lista de todos exceto o último elemento.

    ghci> :type init
    init :: [a] -> [a]
    ghci> init "bar"
    "ba"

Várias das funções acima se comportam mal em listas vazias, portanto tenha cuidado se você não souber se uma lista está vazia ou não. De que forma é que o mau comportamento delas?

    ghci> head []
    *** Exception: Prelude.head: empty list

Tente cada uma das funções acima, no **ghci**. Quais delas falham dada uma lista vazia?

#### Trabalhar de modo seguro e saudável com funções que causam crashy

Quando queremos usar uma função como a `head`, onde sabemos que poderia explodir em nós se passar em uma lista vazia, a tentação pode inicialmente verificar o comprimento da lista antes de chamarmos `head`. Vamos construir um exemplo artificial para ilustrar o nosso ponto.

```haskell
-- file: ch04/EfficientList.hs
myDumbExample xs = if length xs > 0
                   then head xs
                   else 'Z'
```
Se nós estamos vindo de uma linguagem como Perl ou Python, isso pode parecer uma forma perfeitamente natural para escrever este teste. Nos bastidores, as listas de Python são arrays, Perl arrays são, bem arrays. Então eles necessariamente sabem quanto tempo eles estão, e chamar len (foo) ou escalar (@foo) é uma coisa perfeitamente natural de se fazer. Mas como com muitas outras coisas, não é uma boa ideia transplantar cegamente essa suposição em Haskell.

Nós já vimos a definição algébrica do tipo de dados lista muitas vezes, e sabemos que a lista não armazena seu próprio comprimento explicitamente. Assim, a única maneira de `length` poder operar é percorrer toda a lista.

Portanto, quando apenas nos importamos se uma lista está vazia ou não, a duração da chamada não é uma boa estratégia. Pode potencialmente fazer muito mais trabalho do que queremos, se a lista com a qual estamos trabalhando for finita. Como o Haskell nos permite criar facilmente listas infinitas, um uso descuidado do comprimento pode até resultar em um loop infinito.

Uma função mais apropriada para chamar aqui é null, que é executado em tempo constante. Melhor ainda, usando null faz nosso código indicar a propriedade da lista que realmente nos interessa. Aqui estão duas maneiras aprimoradas de expressar myDumbExample 

```haskell
-- file: ch04/EfficientList.hs
mySmartExample xs = if not (null xs)
                    then head xs
                    else 'Z'

myOtherExample (x:_) = x
myOtherExample [] = 'Z'
```

#### Funções parciais e totais

Funções que só têm valores de retorno definido para um subconjunto de entradas válidas são chamadas de funções _parciais_ (chamar `error` não se qualifica como retornar um valor!). Nós chamamos funções que retornam resultados válidos para todos os valores de seu domínio de funções _totais_.

É sempre uma boa ideia saber se uma função que você está usando é parcial ou total. Chamar uma função parcial com uma entrada que ela não pode manipular é provavelmente a maior fonte de bugs diretos e evitáveis nos programas Haskell. 12 comentários

Alguns programadores de Haskell chegam a dar nomes de funções parciais que começam com um prefixo, como unsafe, para que não possam se atirar no pé acidentalmente. 6 comentários

É sem dúvida uma deficiência do Prelude que define algumas funções parciais “inseguras”, como head , sem também fornecer equivalentes totais “seguras”.

#### Mais manipulações de listas simples

O nome Haskell para a função “append” é `(++)`. 

    ghci> :type (++)
    (++) :: [a] -> [a] -> [a]
    ghci> "foo" ++ "bar"
    "foobar"
    ghci> [] ++ [1,2,3]
    [1,2,3]
    ghci> [True] ++ []
    [True]

A função `concat` recebe uma lista de listas, todas do mesmo tipo, e concatena-os em uma única lista.

    ghci> :type concat
    concat :: [[a]] -> [a]
    ghci> concat [[1,2,3], [4,5,6]]
    [1,2,3,4,5,6]


Ele remove um nível de aninhamento.

    ghci> concat [[[1,2],[3]], [[4],[5],[6]]]
    [[1,2],[3],[4],[5],[6]]
    ghci> concat (concat [[[1,2],[3]], [[4],[5],[6]]])
    [1,2,3,4,5,6]


A função `reverse` retorna os elementos de uma lista em ordem inversa.

    ghci> :type reverse
    reverse :: [a] -> [a]
    ghci> reverse "foo"
    "oof"


Para listas do tipo Bool, as funções `and` e `or`, generalizam as funções de dois argumentos`(&&)` e `(||)`, sobre listas.

    ghci> :type and
    and :: [Bool] -> Bool
    ghci> and [True,False,True]
    False
    ghci> and []
    True
    ghci> :type or
    or :: [Bool] -> Bool
    ghci> or [False,False,False,True,False]
    True
    ghci> or []
    False


Existem ainda outras muito úteis, `all` e `any`, que operam em listas de qualquer tipo. Cada uma toma um predicado como seu primeiro argumento, `all`retorna `True` se o predicado for bem-sucedido em cada elemento da lista, enquanto `any`retorna `True` se o predicado for bem-sucedido em pelo menos um elemento da lista.

    ghci> :type all
    all :: (a -> Bool) -> [a] -> Bool
    ghci> all odd [1,3,5]
    True
    ghci> all odd [3,1,4,1,5,9,2,6,5]
    False
    ghci> all odd []
    True
    ghci> :type any
    any :: (a -> Bool) -> [a] -> Bool
    ghci> any even [3,1,4,1,5,9,2,6,5]
    True
    ghci> any even []
    False

#### Trabalhando com sublistas

A função `take`, que conhecemos no [Cápitulo 2](cap2), retorna uma sublista consistindo dos primeiros _k_ elementos de uma lista. Seu inverso, `drop`, descarta _k_ elementos, desde o início da lista.

    ghci> :type take
    take :: Int -> [a] -> [a]
    ghci> take 3 "foobar"
    "foo"
    ghci> take 2 [1]
    [1]
    ghci> :type drop
    drop :: Int -> [a] -> [a]
    ghci> drop 3 "xyzzy"
    "zy"
    ghci> drop 1 []
    []

A função `splitAt` combina as funções de `take` e `drop`, retorna um par da lista de entrada, dividido em um dado índice.

    ghci> :type splitAt
    splitAt :: Int -> [a] -> ([a], [a])
    ghci> splitAt 3 "foobar"
    ("foo","bar")


As funções `takeWhile` e `dropWhile`tomam predicados: `takeWhile` toma elementos a partir do início de uma lista até o predicado retornar `True`, enquanto `dropWhile` descarta elementos da lista, enquanto o predicado retornar `True`.

    ghci> :type takeWhile
    takeWhile :: (a -> Bool) -> [a] -> [a]
    ghci> takeWhile odd [1,3,5,6,8,9,11]
    [1,3,5]
    ghci> :type dropWhile
    dropWhile :: (a -> Bool) -> [a] -> [a]
    ghci> dropWhile even [2,4,6,7,9,10,12]
    [7,9,10,12]


Assim como `splitAt`“tuplas” os resultados de `take` e `drop`, as funções `break` (que já vimos na [seção chamada “Warming up: Separação das linhas de texto portavel”](#fp.splitlines "seção chamada “Warming up: Separação das linhas de texto portavel”")) e `span` tupla os resultados de `takeWhile` e `dropWhile`.

Cada função tem um predicado; `break` consome a sua entrada enquanto o predicado falha, enquanto `span` consome enquanto seu predicado tem êxito.

    ghci> :type span
    span :: (a -> Bool) -> [a] -> ([a], [a])
    ghci> span even [2,4,6,7,9,10,11]
    ([2,4,6],[7,9,10,11])
    ghci> :type break
    break :: (a -> Bool) -> [a] -> ([a], [a])
    ghci> break even [1,3,5,6,8,9,10]
    ([1,3,5],[6,8,9,10])


#### Buscando listas

Como já vimos, a função `elem` indica se um valor está presente em uma lista. Ela tem uma função complementar, `notElem`.

    ghci> :type elem
    elem :: (Eq a) => a -> [a] -> Bool
    ghci> 2 `elem` [5,3,2,1,1]
    True
    ghci> 2 `notElem` [5,3,2,1,1]
    False


Para uma pesquisa mais geral, `filter` toma um predicado, e retorna todos os elementos da lista em que o predicado for bem-sucedido.

    ghci> :type filter
    filter :: (a -> Bool) -> [a] -> [a]
    ghci> filter odd [2,4,1,3,6,8,5,7]
    [1,3,5,7]


Em `Data.List`, três predicados, `isPrefixOf`, `isInfixOf` e `isSuffixOf`, nos deixar testar a presença de sublistas dentro de uma dada lista. A maneira mais fácil de usá-los é usando a notação infixa.

A função isPrefixOf informa se seu argumento esquerdo corresponde ao início de seu argumento correto.

    ghci> :module +Data.List
    ghci> :type isPrefixOf
    isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
    ghci> "foo" `isPrefixOf` "foobar"
    True
    ghci> [1,2] `isPrefixOf` []
    False 


A função `isInfixOf` indica se o seu argumento esquerdo é uma sub-lista do seu direito.

    ghci> :module +Data.List
    ghci> [2,6] `isInfixOf` [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9]
    True
    ghci> "funk" `isInfixOf` "sonic youth"
    False

A operação de `isSuffixOf` não deve precisar de qualquer explicação.

    ghci> :module +Data.List
    ghci> ".c" `isSuffixOf` "crashme.c"
    True


#### Trabalhando com muitas listas ao mesmo tempo

A função `zip`recebe duas listas e “combiná-los” em uma única lista de pares. A lista resultante é o mesmo comprimento que o mais curto das duas entradas.

    ghci> :type zip
    zip :: [a] -> [b] -> [(a, b)]
    ghci> zip [12,72,93] "zippity"
    [(12,'z'),(72,'i'),(93,'p')]



Mais útil é `zipWith`, que pega duas listas e aplica uma função para cada par de elementos, gerando uma lista que é do mesmo comprimento que o menor dos dois.

    ghci> :type zipWith
    zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    ghci> zipWith (+) [1,2,3] [4,5,6]
    [5,7,9]

O sistema de tipos de Haskell faz com que seja um desafio interessante escrever funções que recebam números variáveis de argumentos [8]. Então, se quisermos juntar três listas, chamamos zip3 ou zipWith3 e assim por diante até zip7 e zipWith7

#### Funções especiais de manipulação de string

Nós já conhecemos a função padrão `lines` em [a seção chamada “Warming up: Separação das linhas de texto portavel”](#fp.splitlines "a seção chamada “Warming up: Separação das linhas de texto portavel”"), e o seu homólogo padrão, `unlines`. Observe que `unlines` sempre coloca uma nova linha no final do seu resultado.

    ghci> lines "foo\nbar"
    ["foo","bar"]
    ghci> unlines ["foo", "bar"]
    "foo\nbar\n"


A função `words` divide uma seqüência de entrada em qualquer espaço em branco. Sua contraparte, `unwords`, usa um único espaço para juntar  uma lista de palavras.

    ghci> words "the  \r  quick \t  brown\n\n\nfox"
    ["the","quick","brown","fox"]
    ghci> unwords ["jumps", "over", "the", "lazy", "dog"]
    "jumps over the lazy dog"



### Exercícios

**1.** Escreva suas próprios definições “seguras” das funções de lista parciais, mas certifique-se que a sua nunca falha. Como dica, você pode querer considerar usando os seguintes tipos.

```haskell
-- file: ch04/ch04.exercises.hs
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]
```

**2.** Escreva uma função `splitWith` que atua de forma semelhante a `words`, mas leva um predicado e uma lista de qualquer tipo, e divide sua lista de entrada em cada elemento para o qual o predicado retornar `False`.

```haskell
-- file: ch04/ch04.exercises.hs
splitWith :: (a -> Bool) -> [a] -> [[a]]
```

**3.** Usando a estrutura de comando da [seção chamada “Um framework de linha de comando simples”](#fp.framework "seção chamada “Um framework de linha de comando simples”"), escreva um programa que imprime a primeira palavra de cada linha de sua entrada.

**4.** Escreva um programa que transpõe o texto em um arquivo. Por exemplo, ele deve converter `"hello\nworld\n"` para `"hw\neo\nlr\nll\nod\n"`.

### Como pensar a respeito de loops

Diferentemente das linguagens tradicionais, Haskell não tem nem um `for` loop nem `while` loop. Se nós temos um monte de dados para processar, o que iremos usar no lugar? Existem várias respostas possíveis a esta pergunta.

#### Recursão explícita

Uma maneira simples de fazer o salto de uma linguagem que tem loops para uma que não não tem é através de alguns exemplos, olhando para as diferenças. Aqui está uma função C que pega uma string de dígitos decimais e os transforma em um inteiro.

```c
int as_int(char *str)
{
    int acc; /* accumulate the partial result */

    for (acc = 0; isdigit(*str); str++) {
	acc = acc * 10 + (*str - '0');
    }

    return acc;
}
```

Dado que o Haskell não tem nenhuma construção de loop, como deveríamos pensar em representar um pedaço de código bastante simples como este?

Nós não temos de começar por escrever uma assinatura de tipos, mas ajuda a nos lembrar em que estamos trabalhando.

```haskell
-- file: ch04/IntParse.hs
import Data.Char (digitToInt) -- we'll need ord shortly

asInt :: String -> Int
```
O código C calcula o resultado de forma incremental, a medida que percorre a string; o código Haskell pode fazer o mesma processo. No entanto, em Haskell, podemos expressar o equivalente a um loop como uma funcão. Vamos chamar o nosso `loop` só para manter as coisas agradáveis e explícita.

```haskell
-- file: ch04/IntParse.hs
loop :: Int -> String -> Int

asInt xs = loop 0 xs
```
Esse primeiro parâmetro para `loop` é a variável acumulador que estaremos usando. Passando em zero é equivalente a inicialização do `acc` variável em C, no início do loop.

Ao invés de pular em código chamas, vamos pensar sobre os dados que temos para trabalhar. Nossa String familiar é apenas um sinônimo para \[Char\], uma lista de caracteres. A maneira mais fácil de obtermos a travessia correta é pensar na estrutura de uma lista: ela está vazia ou um único elemento é seguido pelo restante da lista. 

Podemos expressar esse pensamento estrutural diretamente por correspondência de padrões nos construtores do tipo de lista. É sempre útil pensar nos casos fáceis primeiro: aqui, isso significa que consideraremos o caso da lista vazia. 

```haskell
-- file: ch04/IntParse.hs
loop acc [] = acc
```

Uma lista vazia não significa apenas “o String de entrada está vazia”; é também o caso, vamos encontrar quando percorremos todo o caminho até o fim de uma lista não-vazia. Então, nós não queremos que seja tratado como “erro” quando recebemos uma lista vazia. Em vez disso, devemos fazer algo mais coerente. Aqui, a única coisa coerente é terminar o ciclo, e voltar o nosso valor acumulado.

O outro caso que temos que considerar surge quando a lista de entrada não estiver vazia. Precisamos fazer alguma coisa com o elemento atual da lista, e algo com o resto da lista.

```haskell
-- file: ch04/IntParse.hs
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs
```

Nós calculamos um novo valor para o acumulador e damos o nome acc '. Em seguida, chamamos a função loop novamente, passando o valor atualizado acc 'e o resto da lista de entrada; isso é equivalente ao loop escrito em C.

![[Note]](support/figs/note.png)

>**As aspas simples em nomes de variáveis**

>Lembre-se, uma aspa simples é um caractere legal para ser usado em um nome de variável do Haskell e é pronunciado como “primo”. Há um idioma comum em programas do Haskell envolvendo uma variável, digamos foo, e outra variável, digamos foo '. Normalmente podemos supor que foo 'é de alguma forma relacionado a foo. Geralmente é um novo valor para foo, como em nosso código acima. 15 comentários

>Às vezes, vemos esse idioma estendido, como foo ''. Como o rastreamento do número de aspas simples no final de um nome rapidamente se torna tedioso, o uso de mais de duas seguidas é felizmente raro. De fato, até mesmo uma simples citação pode ser fácil de perder, o que pode levar a confusão por parte dos leitores. Pode ser melhor pensar no uso de citações simples como uma convenção de codificação que você deve ser capaz de reconhecer e menos como uma que você realmente deveria seguir.

Cada vez que a função `loop` chama a si mesmo, tem um novo valor para o acumulador, e consome um elemento da lista de entrada. Eventualmente, ela vai chegar ao final da lista, nesse momento o padrão `[]` irá casar, e as chamadas recursivas cessarão.

Quão bem esta função funciona? Para inteiros positivos, é perfeitamente cromulento. 

    ghci> asInt "33"
    33

Mas como estávamos nos concentrando em como atravessar as listas, e não no tratamento de erros, nossa má função se comportaria mal se tentássemos alimentá-la com bobagens.

    ghci> asInt ""
    0
    ghci> asInt "potato"
    *** Exception: Char.digitToInt: not a digit 'p'


Vamos adiar a correção das deficiências da nossa função para Q: 1. 

Porque a última coisa que `loop` faz é simplesmente chamar a si mesma, é um exemplo de uma função recursiva de cauda. Há um outro idioma comum neste código, também. Pensando sobre a estrutura da lista, e o manuseio dos casos de vazio e não vazio separadamente, é um tipo de abordagem chamada de _recursão estrutural_.

Chamamos o caso não-recursivo (quando a lista estiver vazia) e de o _caso de base_(por vezes o _caso de terminação_). Vamos ver as pessoas se referirem ao caso em que a função chama a si mesmo como o caso recursivo (surpresa!), Ou eles podem dar um aceno para a indução matemática e chamá-lo _caso indutivo_.

Como uma técnica útil, recursão estrutural não está confinada a lista, podemos usá-lo em outros tipos de dados algébricos, também. Teremos mais a dizer sobre isso mais tarde.

![[Note]](support/figs/note.png)

>Qual é a grande coisa sobre recursão de cauda?

Em uma linguagem imperativa, um loop é executado em espaço constante. Faltando loops, usamos funções recursivas de cauda em Haskell. Normalmente, uma função recursiva aloca algum espaço cada vez que se aplica, portanto, sabe para onde retornar.

Claramente, uma função recursiva estaria em grande desvantagem em relação a um loop se alocasse memória para cada aplicativo recursivo: isso exigiria espaço linear em vez de espaço constante. No entanto, as implementações de linguagem funcional detectam os usos da recursão de cauda e transformam as chamadas recursivas de cauda a serem executadas em espaço constante; isso é chamado de "tail call optimisation", TCO abreviado. 

Poucas implementações de linguagem imperativa executam o TCO; É por isso que usar qualquer tipo de estilo ambiciosamente funcional em uma linguagem imperativa geralmente leva a vazamentos de memória e baixo desempenho.

#### Transformando cada entrada

Considere uma outra função C, `square`, que eleva ao quadrado cada elemento de um array.

```c
void square(double *out, const double *in, size_t length)
{
    for (size_t i = 0; i < length; i++) {
	out[i] = in[i] * in[i];
    }
}
```


Este contém um tipo simples e comum de loop, que faz exatamente a mesma coisa a cada elemento do seu array de entrada. Como podemos escrever este loop em Haskell?


```haskell
-- file: ch04/Map.hs
square :: [Double] -> [Double]

square (x:xs) = x*x : square xs
square []     = []
```

Nossa função `square` consiste em duas equações com casamento de padrões. A primeira "desconstrói" o começo de uma lista não vazia, para obter sua cabeça e cauda. Ela eleva ao quadrado o primeiro elemento, depois coloca isso na frente de uma nova lista, que é construída chamando square para o restante da lista. A segunda equação assegura que `square` seja interrompido quando atingir o final da lista de entrada.

O efeito de `square` é construir uma nova lista com o mesmo tamanho de sua lista de entrada, com cada elemento na lista de entrada substituído por seu quadrado na lista de saída. 

Aqui está outro loop C, que assegura que cada letra em uma string seja convertida em maiúscula.


```c
#include <ctype.h>

char *uppercase(const char *in)
{
    char *out = strdup(in);
    
    if (out != NULL) {
	for (size_t i = 0; out[i] != '\0'; i++) {
	    out[i] = toupper(out[i]);
	}
    }

    return out;
}
```

Vamos olhar um equivalente Haskell.

```haskell
-- file: ch04/Map.hs
import Data.Char (toUpper)

upperCase :: String -> String

upperCase (x:xs) = toUpper x : upperCase xs
upperCase []     = []
```

Aqui, nós estamos importando a função `toUpper` do módulo padrão `Data.Char`, que contém uma grande quantidade de funções úteis para trabalhar com dados Char.

Nossa função `upperCase` segue um padrão semelhante à nossa função anterior `square`. Ela termina com uma lista vazia quando a lista de entrada está vazia, e quando a entrada não estiver vazia, ela chama `toUpper` no primeiro elemento, em seguida, constrói uma nova lista de com o resultado de chamar-se sobre o resto da entrada da lista.

Estes exemplos seguem um padrão comum para escrever funções recursivas sobre listas em Haskell. O _caso base_ lida com a situação onde a nossa entrada lista está vazia. O _caso recursivo_ trata de uma lista não-vazia, que faz algo com a cabeça da lista, e se chama recursivamente na cauda.

#### Mapeando sobre uma lista

As funções `quadrado` e `maiuscula` que nós definimos produzir novas listas, que são os mesmos comprimentos de suas listas de entrada, e não apenas uma peça de trabalho por elemento. Esse é um padrão comum que prelúdio Haskell define uma função, `map`, para torná-lo mais fácil. `map` tem uma função, e aplica a cada elemento de uma lista, retornando uma nova lista construída a partir dos resultados dessas aplicações.

Aqui estão as nossas funções `square` e `maiuscula` reescrito para usar `map`.

```haskell
-- file: ch04/Map.hs
square2 xs = map squareOne xs
    where squareOne x = x * x

upperCase2 xs = map toUpper xs
```


Este é o nosso primeiro olhar de perto uma função que recebe outra função como argumento. Podemos aprender muito sobre o `map` simplesmente inspecionando seu tipo.

```haskell
    ghci> :type map
    map :: (a -> b) -> [a] -> [b]
```

A assinatura nos diz que `map` tem dois argumentos. A primeira é uma função que assume um valor de um tipo, `a`, e retorna um valor de outro tipo, `b`.

Desde `map` tem uma função como argumento, nós nos referimos a ela como uma função _higher-order_ (Apesar do nome, não há nada de misterioso sobre funções de ordem superior, é apenas um termo para funções que recebem outras funções como argumentos, ou funções de retorno.). 

Desde `map` resumos o padrão comum para as nossas funções `square` e `maiuscula` para que possamos reutilizá-lo com menos clichê, podemos olhar para o que essas funções têm em comum e descobrir como implementá-lo nós mesmos.

```haskell
-- file: ch04/Map.hs
myMap :: (a -> b) -> [a] -> [b]

myMap f (x:xs) = f x : myMap f xs
myMap _ _      = []
```

![[Note]](support/figs/note.png)

>Quais são esses wild cards que fazem lá?

>Se você é novo em programação funcional, as razões para os padrões de correspondência de certas maneiras, nem sempre são óbvias. Por exemplo, na definição de `meuMap` acima, a primeira equação liga a função que está mapeando a variável `f`, mas o segundo usa cartões selvagens para ambos os parâmetros. O acontecendo estabele que?

>Nós usamos um wild card no lugar de `f` para indicar que não estamos chamando a função `f` no lado direito da equação. E sobre a lista de parâmetros? O tipo de lista tem dois construtores. Nós já encontrados no construtor não vazia na primeira equação que define `myMap`. Por eliminação, o construtor da segunda equação é necessariamente o construtor lista vazia, então não há necessidade de realizar um jogo para ver o que realmente é o seu valor.

>Por uma questão de estilo, é bom para usar wild cards para o bem conhecido tipos simples, como listas e Maybe. Para obter mais ou menos complicados tipos familiares, pode ser mais seguro e mais legível o nome construtores explicitamente.

Procuramos a nossa função `meuMap` para nos dar alguma garantia de que ele se comporta de forma semelhante ao `map`padrão.


    ghci> :module +Data.Char
    ghci> map toLower "SHOUTING"
    "shouting"
    ghci> myMap toUpper "whispering"
    "WHISPERING"
    ghci> map negate [1,2,3]
    [-1,-2,-3]

Este padrão de manchas um idioma repetida, então abstraí-lo para que possamos reutilizar (e escrever menos!) De código, é um aspecto comum de programação Haskell. Enquanto a abstração não é exclusivo para Haskell, funções de ordem superior tornam extremamente fácil.

### Seleção de entrada

Outra operação comum em uma seqüência de dados é um pente fino nele para os elementos que satisfaçam algum critério. Aqui está uma função que percorre uma lista de números e retorna aqueles que são estranhos. O nosso código tem um caso recursivo que é um pouco mais complexo do que nossas funções anteriores: ele só coloca um número na lista, ele retorna se o número for ímpar. Usando um guarda expressa muito bem isso.

```haskell
    -- file: ch04/Filter.hs
    oddList :: [Int] -> [Int]

    oddList (x:xs) | odd x     = x : oddList xs
                   | otherwise = oddList xs
    oddList _                  = []
```

Vamos ver isso em ação.

    ghci> oddList [1,1,2,3,5,8,13,21,34]
    [1,1,3,5,13,21]

Mais uma vez, essa expressão é tão comum que o Prelude define uma função, `filter`, que já introduziu. Ele elimina a necessidade de código clichê para recurse sobre a lista.

    ghci> :type filter
    filter :: (a -> Bool) -> [a] -> [a]
    ghci> filter odd [3,1,4,1,5,9,2,6,5]
    [3,1,1,5,9,5]

A função `filter` tem um predicado e aplica a cada elemento em sua lista de entrada, retornando uma lista de apenas aqueles para os quais o predicado avaliar para `True`. Nós iremos rever `filter` novamente em breve, na [seção chamada “Folding da direita”](#fp.foldr.filter "seção chamada “Folding da direita”").

#### Computing uma resposta sobre um conjunto

Outra coisa comum de se fazer com uma coleção é reduzi-lo a um único valor. Um exemplo simples disso é somar os valores de uma lista.
```haskell
-- file: ch04/Sum.hs
mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _      = acc
```

Nossa função `ajudante` é cauda recursiva, e usa um parâmetro acumulador, `acc`, para segurar a soma das correntes parciais da lista. Como já vimos com `asInt`, esta é uma forma “natural” para representar um loop em uma linguagem puramente funcional.

Para algo um pouco mais complicado, vamos dar uma olhada na soma de verificação Adler-32. Este é um algoritmo de soma de verificação popular, que concatena duas somas de 16 bits em um soma de verificação de 32 bits único. A primeira verificação é a soma de todos os bytes de entrada, mais um. A segunda é a soma de todos os valores intermediários da soma primeiro. Em cada caso, as somas são calculadas modulo 65521. Aqui está uma simples, a aplicação Java unoptimised. (É seguro ignorar isso se você não ler Java.)

```java
public class Adler32 
{
    private static final int base = 65521;

    public static int compute(byte[] data, int offset, int length)
    {
	int a = 1, b = 0;

	for (int i = offset; i < offset + length; i++) {
	    a = (a + (data[i] & 0xff)) % base;
	    b = (a + b) % base;
	}

	return (b << 16) | a;
    }
}
```


Apesar de Adler-32 é uma soma simples, esse código não é muito fácil de ler por conta do bit girando envolvidos. Podemos fazer melhor com uma implementação de Haskell?

```haskell
-- file: ch04/Adler32.hs
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

base = 65521

adler32 xs = helper 1 0 xs
    where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                  b' = (a' + b) `mod` base
                              in helper a' b' xs
          helper a b _     = (b `shiftL` 16) .|. a
```

Este código não é exatamente fácil de seguir do que o código Java, mas vamos olhar o que está acontecendo. Primeiro de tudo, nós introduzimos algumas novas funções. A função `shiftL` implementa um deslocamento lógico à esquerda; `(.&.)` fornece bit a bit “e”; e prevê `(.|.)` bit a bit “ou”.

Mais uma vez, a nossa função `ajudante` é recursiva cauda. Nós viramos as duas variáveis, atualizado em cada iteração do loop em Java em parâmetros acumulador. Quando o nosso recursão termina no final da lista de entrada, calculamos nosso soma de verificação e devolvê-lo.

Se dermos um passo para trás, podemos reestruturar nossas Haskell adler32 para ser mais semelhante a nossa função `meuSoma` anterior. Em vez de dois parâmetros acumulador, pode-se usar um par como o acumulador.

```haskell
-- file: ch04/Adler32.hs
adler32_try2 xs = helper (1,0) xs
    where helper (a,b) (x:xs) =
              let a' = (a + (ord x .&. 0xff)) `mod` base
                  b' = (a' + b) `mod` base
              in helper (a',b') xs
          helper (a,b) _     = (b `shiftL` 16) .|. a
```

Por que nós queremos fazer essa mudança, aparentemente sem sentido estrutural? Porque, como já vimos com `map` e `filter`, podemos extrair o comportamento comum compartilhado por `meuSoma` e `adler32_2` em uma função de ordem superior. Podemos descrever esse comportamento como “fazer alguma coisa para cada elemento de uma lista, atualizando um acumulador em que estamos, e retornando o acumulador quando nós somos feitos”.

Este tipo de função é chamado de _fold_, porque ela “dobra” de uma lista. Existem dois tipos de fold sobre listas, `foldl` para dobrar à esquerda (no início) e `foldr` para dobrar a partir da direita (o fim).

#### A fold esquerda

Aqui está a definição de `foldl`.

```haskell
-- file: ch04/Fold.hs
foldl :: (a -> b -> a) -> a -> [b] -> a

foldl step zero (x:xs) = foldl step (step zero x) xs
foldl _    zero []     = zero
```

A função `foldl` leva um função de “passo”, um valor inicial para o acumulador e uma lista. O “passo” leva um acumulador e um elemento da lista, e retorna um valor acumulador novo. Todo `foldl` é chamar o “passo” no acumulador atual e um elemento da lista, e passa o valor do acumulador novo para si mesmo recursivamente para consumir o restante da lista.

Referimo-nos a `foldl` como a “fold esquerda” porque consome a lista da esquerda (a cabeça) para a direita.

Aqui está uma regravação de `meuSoma` usando `foldl`.

```haskell
-- file: ch04/Sum.hs
foldlSum xs = foldl step 0 xs
    where step acc x = acc + x
```

Essa função local `passo` apenas soma dois números, então vamos simplesmente usar o operador de adição ao invés, e eliminar a cláusula desnecessária `where`clause. 

```haskell
-- file: ch04/Sum.hs
niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs
```

Observe como muito mais simples deste código é que o nosso `meuSoma` original? Não estamos mais usando recursão explícita, porque `foldl` cuida disso para nós. Nós simplificamos o nosso problema para baixo a duas coisas: que o valor inicial do acumulador deve ser (o segundo parâmetro para `foldl`), e como atualizar o acumulador (a função `(+)`). Como um bônus adicional, o nosso código é agora mais curto, também, o que torna mais fácil de entender.

Vamos ter um olhar mais profundo que `foldl` está fazendo aqui, manualmente, escrevendo cada etapa em sua avaliação, quando chamamos `somaFina [1,2,3]`. 

```haskell
-- file: ch04/Fold.hs
foldl (+) 0 (1:2:3:[])
          == foldl (+) (0 + 1)             (2:3:[])
          == foldl (+) ((0 + 1) + 2)       (3:[])
          == foldl (+) (((0 + 1) + 2) + 3) []
          ==           (((0 + 1) + 2) + 3)
```


Podemos reescrever `adler32_2` usando `foldl` deixar-nos concentrar nos detalhes que são importantes.

```haskell
-- file: ch04/Adler32.hs
adler32_foldl xs = let (a, b) = foldl step (1, 0) xs
                   in (b `shiftL` 16) .|. a
    where step (a, b) x = let a' = a + (ord x .&. 0xff)
                          in (a' `mod` base, (a' + b) `mod` base)
```

Aqui, o nosso acumulador é um par, assim que o resultado de `foldl` será, também. Puxamos o acumulador final distante quando retorna `foldl` e-bit mexer-lo em uma soma de verificação “apropriada”.

#### Por que usar folds, maps e filters?

Uma rápida olhada revela que `adler32_foldl` não é realmente menor do que qualquer `adler32_2`. Por que devemos usar uma dobra neste caso? A vantagem reside no fato de que as dobras são extremamente comuns em Haskell, e eles têm um comportamento regular e previsível.

Isso significa que um leitor com um pouco de experiência terão um tempo mais fácil o entendimento a utilização de uma prega que o código que usa recursão explícita. A dobra não vai produzir nenhuma surpresa, mas o comportamento de uma função que recursivamente explicitamente não é imediatamente óbvio. recursão explícita nos obriga a ler atentamente para entender exatamente o que está acontecendo.

Esta linha de raciocínio se aplica a outras funções de biblioteca de ordem superior, incluindo aqueles que já vimos, `map` e `filter`. Porque eles são bibliotecas de funções com comportamentos bem definidos, só precisamos saber o que eles fazem uma vez, e nós vamos ter uma vantagem quando nós precisamos de compreender qualquer código que usa-los. Estas melhorias na legibilidade também transitar para escrever código. Assim que começar a pensar com funções de ordem superior em mente, vamos produzir um código conciso mais rapidamente.

##### Folding da direita

A contrapartida `foldl` é `foldr`, a que dobra a partir da direita de uma lista.

```haskell
 -- file: ch04/Fold.hs
foldr :: (a -> b -> b) -> b -> [a] -> b

foldr step zero (x:xs) = step x (foldr step zero xs)
foldr _    zero []     = zero
```

Vamos seguir o mesmo processo de avaliação manual com `foldr (+) 0 [1,2,3]` como fizemos com `somaFina` na [seção chamada “A fold esquerda”](#fp.foldl "seção chamada “A fold esquerda”"). 

```haskell
-- file: ch04/Fold.hs
foldr (+) 0 (1:2:3:[])
          == 1 +           foldr (+) 0 (2:3:[])
          == 1 + (2 +      foldr (+) 0 (3:[])
          == 1 + (2 + (3 + foldr (+) 0 []))
          == 1 + (2 + (3 + 0))
```

A diferença entre `foldl` e `foldr`deve ser claro de olhar para onde os parênteses e os “lista vazia” elementos aparecem. Com `foldl`, o elemento da lista é vazia à esquerda, e todo o grupo parênteses à esquerda. Com `foldr`, o valor `zero` é à direita, eo grupo de parênteses para a direita.

Há uma explicação intuitiva linda de como `foldr` funciona: ele substitui a lista vazia com o valor `zero`, e cada construtor na lista com uma aplicação da função de passo.

```haskell
-- file: ch04/Fold.hs
1 : (2 : (3 : []))
1 + (2 + (3 + 0 ))
```

À primeira vista, `foldr` pode parecer menos úteis do que `foldl`: o uso que é uma função que se dobra a partir da direita? Mas considere a função `filter` do Prelude, que a última vez que encontrou na [seção chamada “Seleção de peças de entrada”](#fp.filter "seção chamada “Seleção de peças de entrada”"). Se escrevermos `filter` usando recursão explícita, será algo parecido com isso.

```haskell
-- file: ch04/Fold.hs
filter :: (a -> Bool) -> [a] -> [a]
filter p []   = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
```    

Talvez de forma surpreendente, no entanto, pode-se escrever `filter` como um fold, usando `foldr`.

```haskell
-- file: ch04/Fold.hs
myFilter p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys
```

Este é o tipo de definição que poderia nos causar uma dor de cabeça, por isso vamos examiná-lo em um pouco de profundidade. Como `foldl`, `foldr` tem uma função e um caso-base (o que fazer quando a lista de entrada está vazia) como argumentos. Da leitura do tipo de `filter`, nós sabemos que a nossa função `meuFilter` deve retornar uma lista do mesmo tipo que ele consome, então o caso de base deve ser uma lista desse tipo e, a função de auxiliar `passo` deve retornar uma lista.

Como sabemos que `foldr` calls `passo` convida um elemento da lista de entrada de cada vez, com o acumulador como seu segundo argumento, o `passo` deve ser muito simples. Se o predicado retorna `True`, ele empurra esse elemento para a lista acumulados, caso contrário, ele sai da lista intocada.

A classe de funções que podemos expressar utilizando `foldr` é chamada _recursiva primitiva_. Um número surpreendentemente grande de funções de manipulação de lista são recursivas primitivas. Por exemplo, aqui está a `map` escrita em termos de `foldr`.

```haskell
-- file: ch04/Fold.hs
myMap :: (a -> b) -> [a] -> [b]

myMap f xs = foldr step [] xs
    where step x ys = f x : ys
```

Na verdade, podemos até escrever `foldl` usando `foldr`!

```haskell
-- file: ch04/Fold.hs
myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)
```


![[Tip]](support/figs/tip.png)

>Compreender foldl em termos de foldr

>Se você quiser definir-se um desafio contínuo, tente seguir a definição acima de `foldl` usando `foldr`. Esteja avisado: esta não é trivial! Você pode querer ter as seguintes ferramentas na mão: algumas pílulas de dor de cabeça e um copo de água, **ghci**(para que você possa descobrir o que faz a função `id`), e um lápis e papel.

>Você vai querer seguir o mesmo processo de avaliação manual como descrito acima para ver o que `foldl` e `foldr` estavam realmente fazendo. Se você ficar preso, você pode encontrar a tarefa mais fácil, depois de ler a [seção chamada “Aplicação da função parcial e currying”](#fp.partialapp "seção chamada “Aplicação da função parcial e currying”").

Voltando à nossa explicação anterior `foldr` intuitiva do que faz, uma outra maneira útil de pensar sobre isso é que ele _transforma_ sua entrada de lista. Os dois primeiros argumentos são “o que fazer com cada elemento da cauda da cabeça / da lista”, e “o que para substituir o final da lista”.

A “identidade” transformação com `foldr`assim substitui a lista vazia com ela mesma, e aplica-se o construtor de lista para cada cabeça / cauda par:x

```haskell
-- file: ch04/Fold.hs
identity :: [a] -> [a]
identity xs = foldr (:) [] xs
```
Ela transforma uma lista em uma cópia de si mesmo.


    ghci> identity [1,2,3]
    [1,2,3]

Se `foldr` substitui o fim de uma lista com algum outro valor, isto dá-nos uma outra maneira de olhar para afunção Haskell de acréscimo das listas, `(++)`.

    ghci> [1,2,3] ++ [4,5,6]
    [1,2,3,4,5,6]


Tudo o que temos de fazer para anexar uma lista para outra é substituir essa lista segundo para o fim da nossa primeira lista.

```haskell
-- file: ch04/Fold.hs
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs
```

Vamos tentar fazer isso.

    ghci> append [1,2,3] [4,5,6]
    [1,2,3,4,5,6]

Aqui, podemos substituir cada construtor lista com outro construtor da lista, mas substituir a lista vazia com a lista que deseja acrescentar sobre o fim da nossa primeira lista.

Como o nosso tratamento prolongado das pregas devem indicar, a função `foldr` é quase tão importante membro da nossa caixa de ferramentas de programação lista das funções mais básicas lista vimos na [seção chamada “Trabalhar com as listas”](#fp.lists "seção chamada “Trabalhar com as listas”"). Pode consumir e produzir uma lista de forma incremental, o que o torna útil para a gravação de dados preguiçoso código de processamento.

#### Folds a esquerda, avalaiação preguiça e space leaks

Para manter o nosso simples discussão inicial, usamos `foldl` durante a maior parte desta seção. Isso é conveniente para o teste, mas nunca iremos usar `foldl` na prática.

A razão tem a ver com a avaliação não-estrita Haskell. Se aplicarmos `foldl (+) [1,2,3]`, que avalia a expressão `(((0 + 1) + 2) + 3)`. Podemos ver isso acontecer se rever a forma como a função é expandida.

```haskell
-- file: ch04/Fold.hs
foldl (+) 0 (1:2:3:[])
          == foldl (+) (0 + 1)             (2:3:[])
          == foldl (+) ((0 + 1) + 2)       (3:[])
          == foldl (+) (((0 + 1) + 2) + 3) []
          ==           (((0 + 1) + 2) + 3)
```

A expressão final não será avaliada a `6` até que seu valor é exigido. Antes que seja avaliada, ele deve ser armazenado como uma conversão. Não surpreendentemente, uma conversão é mais caro do que guardar um número único, e os mais complexos a expressão thunked, o espaço mais precisa. Para algo mais barata, como aritmética, thunking um expressão é computacionalmente mais caro do que avaliá-lo imediatamente. Temos, assim, acabam pagando tanto no espaço quanto no tempo.

Quando GHC está avaliando uma expressão thunked, ele usa uma pilha interna para isso. Como uma expressão thunked poderia ser infinitamente grande, o GHC coloca um limite fixo sobre o tamanho máximo da pilha. Graças a esse limite, podemos tentar uma expressão de grande thunked em **ghci** sem precisar se preocupar que ele possa consumir toda a memória.

    ghci> foldl (+) 0 [1..1000]
    500500

De olhar para a expansão acima, podemos supor que este cria uma conversão que consiste em 1.000 inteiros e 999 pedidos de `(+)`. Isso é um monte de memória e esforço para representar um único número! Com uma expressão maior, embora o tamanho ainda é modesta, os resultados são mais dramáticos.

    ghci> foldl (+) 0 [1..1000000]
    *** Exception: stack overflow

Em expressões pequenas, `foldl` irá funcionar corretamente, mas lentamente, devido à sobrecarga thunking em que incorre. Nós nos referimos a este thunking invisível como um _space leak_(vazamento de espaço), porque o nosso código está funcionando normalmente, mas com muito mais memória do que deveria.

Em expressões maiores, código com um vazamento de espaço simplesmente falham, como acima. Um space leak com `foldl` é um obstáculo clássico para novos programadores Haskell. Felizmente, isso é fácil de evitar.

O módulo `Data.List` define uma função chamada `foldl'` que é semelhante ao `foldl`, mas não construir thunks. A diferença de comportamento entre os dois é óbvia.

    ghci> foldl  (+) 0 [1..1000000]
    *** Exception: stack overflow
    ghci> :module +Data.List
    ghci> foldl' (+) 0 [1..1000000]
    500000500000

Devido ao comportamento de thunking de `foldl`, é prudente evitar essa função em programas reais: mesmo que não falham completamente, será desnecessariamente ineficiente. Em vez disso, importa `Data.List` e utilisa `foldl'`.

### Exercícios

**1.** Use uma fold (escolhendo o fold adequada fará seu código muito mais simples) para reescrever e melhorar a função `asInt` da [secção chamada “Recursão explícita”](#fp.tailrecursion "secção chamada “Recursão explícita”").

```haskell
-- file: ch04/ch04.exercises.hs
asInt_fold :: String -> Int
```

Sua função deve se comportar como se segue.

    ghci> asInt_fold "101"
    101
    ghci> asInt_fold "-31337"
    -31337
    ghci> asInt_fold "1798"
    1798

Estenda a sua função para tratar os seguintes tipos de condições excepcionais chamando `error`.

    ghci> asInt_fold ""
    0
    ghci> asInt_fold "-"
    0
    ghci> asInt_fold "-3"
    -3
    ghci> asInt_fold "2.7"
    *** Exception: Char.digitToInt: not a digit '.'
    ghci> asInt_fold "314159265358979323846"
    564616105916946374

**2.** A função `asInt_fold` usa `error`, por isso seus chamadores não pode manipular erros. Reescrevê-lo para corrigir esse problema.

```haskell
-- file: ch04/ch04.exercises.hs
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
```

    ghci> asInt_either "33"
    Right 33
    ghci> asInt_either "foo"
    Left "non-digit 'o'"



**3.** A função Prelude `concat`concatena uma lista de listas em uma única lista, e digite o seguinte.

```haskell
-- file: ch04/ch04.exercises.hs
concat :: [[a]] -> [a]
```

Escreva a sua própria definição de `concat` usando `foldr`. 

**4.** Escreva a sua própria definição da função padrão `takeWhile`, primeiro usando recursão explícita, então `foldr`.

**5.** O módulo `Data.List` define uma função, `groupBy`, que tem o seguinte tipo.

```haskell
-- file: ch04/ch04.exercises.hs
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
```

Use **ghci** para carregar o módulo `Data.List` e descobrir o que `groupBy` faz, em seguida, escrever sua própria implementação usando uma fold.

**6.** Quantas das seguintes funções Prelude pode-se reescrever usando dobras lista?

*   `any`
    
*   `cycle`
    
*   `words`
    
*   `unlines`
    
Para essas funções, onde você pode usar tanto `foldl'` ou `foldr`, que é mais adequado em cada caso?

##### Leitura complementar

O artigo \[[Hutton99](bibliography.html#bib.hutton99 "[Hutton99]")\] é um excelente e profundo tutorial pregas cobertura. Ele inclui muitos exemplos de como usar técnicas simples e sistemática de cálculo para transformar funções que usam recursão explícita em folds.

### Funções anónimas (lambda)

Em muitas das definições de funções que vimos até agora, nós escrevemos funções de auxiliar de curta duração.

```haskell
-- file: ch04/Partial.hs
isInAny needle haystack = any inSequence haystack
    where inSequence s = needle `isInfixOf` s
```

Haskell lets us write completely anonymous functions, which we can use to avoid the need to give names to our helper functions. Anonymous functions are often called “lambda” functions, in a nod to their heritage in the lambda calculus. We introduce an anonymous function with a backslash character, `\`, pronounced _lambda_\[[9](#ftn.id594951)\]. This is followed by the function's arguments (which can include patterns), then an arrow `->`to introduce the function's body. 

Lambdas are most easily illustrated by example. Here's a rewrite of `isInAny`using an anonymous function. 

```haskell
-- file: ch04/Partial.hs
isInAny2 needle haystack = any (\s -> needle `isInfixOf` s) haystack
```

Nós colocaremos o lambda parênteses aqui para que Haskell pode dizer onde o corpo da função termina.

Funções anónimos se comportar de forma idêntica em todos os aspectos das funções que têm nomes, mas Haskell coloca algumas restrições importantes sobre como podemos defini-los. O mais importante, enquanto nós podemos escrever uma função normal usando várias cláusulas contendo diferentes modelos e os guardas, um lambda pode ter apenas uma única cláusula na sua definição.

A limitação a uma única cláusula restringe como podemos usar os padrões na definição de uma lambda. Vamos escrever uma função geralmente normal, com várias cláusulas para cobrir possibilidades diferentes padrões de correspondência.

```haskell
-- file: ch04/Lambda.hs
safeHead (x:_) = Just x
safeHead _ = Nothing
```

Mas como não podemos escrever várias cláusulas para definir uma lambda, devemos estar certos de que qualquer padrão que usamos fósforo.

```haskell
-- file: ch04/Lambda.hs
unsafeHead = \(x:_) -> x
```

Esta definição de `headInseguro` vai explodir em nossas faces se chamá-lo com um valor em que a correspondência de padrão falhar.

    ghci> :type unsafeHead
    unsafeHead :: [t] -> t
    ghci> unsafeHead [1]
    1
    ghci> unsafeHead []
    *** Exception: Lambda.hs:7:13-23: Non-exhaustive patterns in lambda

A definição typechecks, assim ele vai compilar, então o erro irá ocorrer durante a execução. A moral desta história é que ter cuidado em como usar padrões para definir uma função anônima: certifique-se de seus padrões não pode falhar!

Outra coisa a notar sobre as funções `isInAny` e `isInAny2` que mostrei acima é que a primeira versão, usando uma função auxiliar que tem um nome, é um pouco mais fácil de ler que a versão que se estatela uma função anônima para o meio. A função de auxiliar nomeado não perturbar o “fluxo” de a função na qual ele é usado, eo nome escolhido criteriosamente nos dá um pouco de informação sobre o que a função é esperado.

Em contraste, quando corremos em um lambda no meio de um corpo da função, temos de trocar as marchas e ler a sua definição bastante cuidado para entender o que ele faz. Para ajudar com a legibilidade e facilidade de manutenção, então, tendemos a evitar lambdas em muitas situações onde poderíamos utilizá-los para cortar alguns personagens de uma definição de função. Muitas vezes, nós vamos usar uma função parcialmente aplicado em vez disso, resultando em um código mais claro e legível do que qualquer um lambda ou uma função explícita. Não sei o que uma função parcialmente aplicado é ainda? Leia mais!

Nós não pretendemos estas advertências para sugerir que lambdas são inúteis, mas apenas que devemos estar atentos às possíveis armadilhas quando estamos pensando em utilizá-los. Nos capítulos seguintes, veremos que são muitas vezes de valor inestimável como “cola”.

### Aplicação parcial da função e currying

Você pode se perguntar por que a seta `->` é usado para o que parece ser a dois propósitos na assinatura de um tipo de função.

    ghci> :type dropWhile
    dropWhile :: (a -> Bool) -> [a] -> [a]

Parece que o `->` é separar os argumentos para `dropWhile` umas das outras, mas que também separa os argumentos do tipo de retorno. Mas, na verdade `->` tem apenas um significado: ele denota uma função que recebe um argumento do tipo à esquerda, e retorna um valor do tipo do lado direito.

A implicação aqui é muito importante: em Haskell, _todas as funções de tomar apenas um argumento_. Quando `dropWhile` _parece_ como uma função que recebe dois argumentos, é realmente uma função de um argumento, que retorna uma função que recebe um argumento. Aqui está uma expressão perfeitamente válida Haskell.

    ghci> :module +Data.Char
    ghci> :type dropWhile isSpace
    dropWhile isSpace :: [Char] -> [Char]

Bem, _isso_ parece útil. O valor `dropWhile isSpace` é uma função que retira líder espaço em branco de uma string. Como isso é útil? Como exemplo, podemos usá-lo como um argumento para uma função de ordem superior.

    ghci> map (dropWhile isSpace) [" a","f","   e"]
    ["a","f","e"]

Toda vez que nós fornecemos um argumento para uma função, nós podemos “cortar” um elemento fora da parte dianteira de sua assinatura tipo. Vamos tomar como exemplo `zip3` para ver o que queremos dizer, esta é uma função que fecha três listas em uma lista de três tuplas.

    ghci> :type zip3
    zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
    ghci> zip3 "foo" "bar" "quux"
    [('f','b','q'),('o','a','u'),('o','r','u')]


Se aplicarmos `zip3` com apenas um argumento, temos uma função que aceita dois argumentos. Não importa o que nós fornecemos argumentos para esta função compostos, seu primeiro argumento será sempre o valor fixo que especificamos.

    ghci> :type zip3 "foo"
    zip3 "foo" :: [b] -> [c] -> [(Char, b, c)]
    ghci> let zip3foo = zip3 "foo"
    ghci> :type zip3foo
    zip3foo :: [b] -> [c] -> [(Char, b, c)]
    ghci> (zip3 "foo") "aaa" "bbb"
    [('f','a','b'),('o','a','b'),('o','a','b')]
    ghci> zip3foo "aaa" "bbb"
    [('f','a','b'),('o','a','b'),('o','a','b')]
    ghci> zip3foo [1,2,3] [True,False,True]
    [('f',1,True),('o',2,False),('o',3,True)]

Quando passamos menos argumentos para uma função que a função pode aceitar, nós chamamos isso de _aplicação parcial_ da função: estamos aplicando a função a que apenas alguns de seus argumentos.

No exemplo acima, temos uma função aplicada parcialmente, `zip3 "foo"`, e uma nova função, `zip3foo`. Podemos ver que as assinaturas do tipo dois e seu comportamento são idênticos.

Isto aplica-se tão bem se fixar dois argumentos, dando-nos uma função de apenas um argumento.

    ghci> let zip3foobar = zip3 "foo" "bar"
    ghci> :type zip3foobar
    zip3foobar :: [c] -> [(Char, Char, c)]
    ghci> zip3foobar "quux"
    [('f','b','q'),('o','a','u'),('o','r','u')]
    ghci> zip3foobar [1,2]
    [('f','b',1),('o','a',2)]

Aplicação parcial de função nos permite evitar a criação de funções descartáveis cansativo. Muitas vezes é mais útil para este propósito que as funções anônimas que introduzimos na [seção chamada “Funções (lambda) anónimos”](#fp.anonymous "seção chamada “Funções (lambda) anónimos”"). Olhando para trás, a função `isInAny` nós definimos lá, aqui está como nós usaríamos uma função parcialmente aplicado em vez de uma função auxiliar chamada ou uma lambda.

```haskell
-- file: ch04/Partial.hs
isInAny3 needle haystack = any (isInfixOf needle) haystack
```

Aqui, a expressão `isInfixOf needle` é a função aplicada parcialmente. Nós estamos tomando a função `isInfixOf`, e “consertar” seu primeiro argumento a ser a variável de `needle` de nossa lista de parâmetros. Isso nos dá uma função parcialmente aplicada que tem exatamente o mesmo tipo de comportamento e como o ajudante e lambda em nossas definições anteriores.

Aplicação de função parcial é chamado _currying_, após o lógico Haskell Curry (para quem a linguagem Haskell é chamado).

Como outro exemplo de currying em uso, vamos voltar para a função lista-resumo que escrevi na [seção chamada “A fold esquerda”](#fp.foldl "seção chamada “A fold esquerda”").

```haskell
-- file: ch04/Sum.hs
niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs
```

Nós não precisamos de aplicar plenamente `foldl`, podemos omitir a lista de `xs` tanto a lista de parâmetros e os parâmetros para `foldl`, e nós vamos acabar com uma função mais compacto que tem o mesmo tipo.

```haskell
-- file: ch04/Sum.hs
nicerSum :: [Integer] -> Integer
nicerSum = foldl (+) 0
```

#### Secções

Haskell fornece um atalho útil para notação vamos escrever uma função parcialmente aplicadas em estilo infixo. Se colocar um operador em parênteses, nós podemos fornecer o seu argumento a esquerda ou direita dentro dos parênteses para obter uma função aplicada parcialmente. Este tipo de aplicação parcial é chamado de _section_.

    ghci> (1+) 2
    3
    ghci> map (*3) [24,36]
    [72,108]
    ghci> map (2^) [3,5,7,9]
    [8,32,128,512]


Se nos fornecer o argumento à esquerda dentro da seção, chamando a função resultante com um material argumento argumento do lado direito do operador. E vice-versa.

Lembre-se que nós podemos envolver um nome de função em backquotes usá-lo como um operador infixo. Isto nos permite usar seções com funções.

    ghci> :type (`elem` ['a'..'z'])
    (`elem` ['a'..'z']) :: Char -> Bool


A definição acima fixa o segundo argumento de `elem` dando-nos uma função que verifica se seu argumento for uma letra minúscula.

    ghci> (`elem` ['a'..'z']) 'f'
    True

Usando isso como um argumento para `all`, temos uma função que verifica uma seqüência inteira para ver se está tudo em minúsculas.

    ghci> all (`elem` ['a'..'z']) "Frobozz"
    False

Se usarmos esse estilo, podemos melhorar ainda mais a leitura de nossa função `isInAny3` anterior.

```haskell
-- file: ch04/Partial.hs
isInAny4 needle haystack = any (needle `isInfixOf`) haystack
```

### As-patterns


A função Haskell `tails`, no módulo `Data.List`, generaliza a função `tail` foi introduzida recentemente. Em vez de retornar uma “cauda” da lista, ele retorna _todos_ eles.

    ghci> :m +Data.List
    ghci> tail "foobar"
    "oobar"
    ghci> tail (tail "foobar")
    "obar"
    ghci> tails "foobar"
    ["foobar","oobar","obar","bar","ar","r",""]

Cada uma dessas cadeias é um _sufixo_ de String inicial, para `tails` produz uma lista de todos os sufixos, além de uma lista vazia extra no final. Ela produz sempre que a lista extra vazio, mesmo quando sua lista de entrada está vazia.

    ghci> tails []
    [[]]

E se queremos uma função que se comporta como `tails`, mas que retorna _apenas_ os sufixos não vazios? Uma possibilidade seria para nós a escrever a nossa própria versão a mão. Vamos usar uma nova peça de notação, o símbolo `@`.

```haskell
-- file: ch04/SuffixTree.hs
suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []
```

O padrão `xs@(_:xs')` é chamado um _padrão as_, e significa “ligar o variável `xs` para o valor que corresponda ao lado direito do símbolo `@`”.

No nosso exemplo, se o padrão depois do “@” corresponde, `xs` será obrigado a toda a lista que combinava e `xs'` para todos, mas o cabeça da lista (usamos o padrão wild card `_` para indicar que estamos não está interessado no valor do cabeça de lista).

    ghci> tails "foo"
    ["foo","oo","o",""]
    ghci> suffixes "foo"
    ["foo","oo","o"]

O padrão as torna o código nosso mais legível. Para ver como isso ajuda, vamos comparar uma definição que não tenha um padrão as.

    -- file: ch04/SuffixTree.hs
    noAsPattern :: [a] -> [[a]]
    noAsPattern (x:xs) = (x:xs) : noAsPattern xs
    noAsPattern _ = []

Aqui, a lista que nós desconstruído no padrão de jogo só fica colocada de volta em conjunto no corpo da função.

Padrões as ter um uso mais prático do que a leitura simples: eles podem nos ajudar a compartilhar dados, em vez de copiá-lo. Em nossa definição de `semPadrãoAs`, quando jogo `(x:xs)`, vamos construir uma nova cópia dele no corpo da nossa função. Isso nos leva a atribuir um nó nova lista em tempo de execução. Isso pode ser barato, mas não é livre. Em contraste, quando nós definimos `sufixos`, reutilizadas o valor `xs` que nós combinamos com o nosso como padrão. Desde que reutilizar um valor existente, evitamos uma atribuição pouco.

### Reutilização de código através da composição


Parece uma vergonha para introduzir uma nova função, `sufixos`, que faz quase a mesma coisa que a função existente `tails`. Certamente nós podemos fazer melhor?

Lembre-se da função `init` introduzimos na [seção chamada “Trabalhar com as listas”](#fp.lists "seção chamada “Trabalhar com as listas”"): retorna todos, mas o último elemento de uma lista.

```haskell
-- file: ch04/SuffixTree.hs
suffixes2 xs = init (tails xs)
```

Esta função `sufixos2` funciona igualmente a `sufixos`, mas é um única linha de código.


    ghci> suffixes2 "foo"
    ["foo","oo","o"]

Se tomarmos um passo para trás, vemos o reflexo de um padrão aqui: nós estamos aplicando uma função, em seguida, aplicar uma outra função para o seu resultado. Vamos transformar esse padrão em uma definição de função.

```haskell
-- file: ch04/SuffixTree.hs
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
```


Agora temos uma função, `compor`, que podemos usar para “cola” outras duas funções em conjunto.

```haskell
-- file: ch04/SuffixTree.hs
suffixes3 xs = compose init tails xs
```

O currying automático do Haskell nos deixa cair a variável `xs`para que possamos fazer a nossa definição ainda mais curtos.

```haskell
-- file: ch04/SuffixTree.hs
suffixes4 = compose init tails
```

Felizmente, não precisamos de escrever a nossa própria função `compor`. Ligar funções em cada um, como isto é tão comum que a Prelude fornece composição das funções através do operador `(.)`.

```haskell
-- file: ch04/SuffixTree.hs
suffixes5 = init . tails
```

O operador `(.)` não é uma parte especial da sintaxe da linguagem, é apenas um operador normal.

    ghci> :type (.)
    (.) :: (b -> c) -> (a -> b) -> a -> c
    ghci> :type suffixes
    suffixes :: [a] -> [[a]]
    ghci> :type suffixes5
    suffixes5 :: [a] -> [[a]]
    ghci> suffixes5 "foo"
    ["foo","oo","o"]

Podemos criar novas funções a qualquer momento por escrito cadeias de funções compostas, costurado com `(.)`, tanto tempo (é claro) como o tipo de resultado da função no lado direito de cada um `(.)` corresponde ao tipo de parâmetro que o função na esquerda pode aceitar.

Como exemplo, vamos resolver um enigma muito simples: a contagem do número de palavras em uma seqüência que começa com uma letra maiúscula.

    ghci> :module +Data.Char
    ghci> let capCount = length . filter (isUpper . head) . words
    ghci> capCount "Hello there, Mom!"
    2


Podemos entender que esta função é composta pela análise das suas peças. A função `(.)` é associativa direito, por isso vamos prosseguir da direita para a esquerda.

    ghci> :module +Data.Char
    ghci> let capCount = length . filter (isUpper . head) . words
    ghci> capCount "Hello there, Mom!"
    2

A função `words` tem um tipo de resultado de \[String\], para o que está no lado esquerdo de `(.)` deve aceitar um argumento compatível.

    ghci> :type isUpper . head
    isUpper . head :: [Char] -> Bool


Essa função retorna `True` se uma palavra começa com uma letra maiúscula (testá-lo em **ghci**), os `filter (isUpper . head)` retorna uma lista de Strings contendo apenas palavras que começam com letras maiúsculas.

    ghci> :type filter (isUpper . head)
    filter (isUpper . head) :: [[Char]] -> [[Char]]

Uma vez que esta expressão retorna uma lista, tudo o que resta é calcular o comprimento da lista, o que fazemos com outra composição.

Aqui está outro exemplo, retirado de uma aplicação real. Queremos extrair uma lista de nomes de macro de um arquivo de cabeçalho C acompanha `libpcap`, uma biblioteca popular pacote de filtragem de rede. O arquivo de cabeçalho contém um grande número de definições da seguinte forma.

```c
#define DLT_EN10MB      1       /* Ethernet (10Mb) */
#define DLT_EN3MB       2       /* Experimental Ethernet (3Mb) */
#define DLT_AX25        3       /* Amateur Radio AX.25 */
```


Nosso objetivo é extrair nomes como `DLT_EN10MB` e `DLT_AX25`.

```haskell
-- file: ch04/dlts.hs
import Data.List (isPrefixOf)

dlts :: String -> [String]

dlts = foldr step [] . lines
```

Nós tratamos todo um arquivo como uma String, dividi-lo com `lines`, em seguida, aplicar `foldr passo []` para a lista resultante de linhas. A função de auxiliar `passo` opera em uma única linha.

```haskell
-- file: ch04/dlts.hs
  where step l ds
          | "#define DLT_" `isPrefixOf` l = secondWord l : ds
          | otherwise                     = ds
        secondWord = head . tail . words
```

Se coincidir com uma definição de macro com a nossa expressão guarda, podemos contras o nome da macro para a cabeça da lista que está retornando, caso contrário, deixamos a lista intocada.

Enquanto as funções individuais do corpo de `palavra2` estão agora familiar para nós, pode levar um pouco de prática para montar uma cadeia de composições como esta. Vamos examinar o processo.

Mais uma vez, procede da direita para a esquerda. A primeira função é `words`. 

    ghci> :type words
    words :: String -> [String]
    ghci> words "#define DLT_CHAOS    5"
    ["#define","DLT_CHAOS","5"]

Em seguida, aplicamos `tail` para o resultado de `words`.

    ghci> :type tail
    tail :: [a] -> [a]
    ghci> tail ["#define","DLT_CHAOS","5"]
    ["DLT_CHAOS","5"]
    ghci> :type tail . words
    tail . words :: String -> [String]
    ghci> (tail . words) "#define DLT_CHAOS    5"
    ["DLT_CHAOS","5"]

Finalmente, aplicando `head` para o resultado de `drop 1 . words` nos dará o nome de nossa macro.

    ghci> :type head . tail . words
    head . tail . words :: String -> String
    ghci> (head . tail . words) "#define DLT_CHAOS    5"
    "DLT_CHAOS"

#### Use "head" sabiamente

Depois da advertência contra lista de funções inseguras na [seção chamada “Trabalhar segura e saudavelmente a com funções crashy”](#fp.lists.safe "seção chamada “Trabalhar segura e saudavelmente a com funções crashy”"), aqui estamos chamando tanto a `head` e a `tail`, duas dessas funções de lista inseguro. O Que Dá?

Neste caso, podemos nos assegurar de inspeção que estamos seguros de uma falha de execução. O guarda padrão na definição de `passo` contém duas palavras, por isso quando nós aplicamos `words` a qualquer String de palavras que faz passar pelo guarda, que vamos ter uma lista de pelo menos dois elementos, `"#define"` e alguns macro iniciando com `"DLT_"`.

Este tipo de raciocínio que devemos fazer para nos convencermos de que nosso código não vai explodir quando chamamos funções parciais. Não se esqueça nossa admoestação anterior: chamar funções inseguro como este requer cuidados, e muitas vezes pode tornar o código mais frágil de maneira sutil. Se por algum motivo, modificou o padrão de proteção para conter apenas uma palavra, poderíamos nos expor à possibilidade de um acidente, como o corpo da função assume que receberá duas palavras.

### Dicas para escrever código legível


Até agora, neste capítulo, me deparei com duas características tentador olhar de Haskell: recursão de cauda e funções anônimas. Tão agradável como estes são, muitas vezes não se deseja usá-los.

Muitas operações de manipulação de lista pode ser mais facilmente expressos usando combinações de funções de biblioteca, tais como `map`, `take`, e `filter`. Sem dúvida, isto requer alguma prática para se acostumar com estes. No retorno para nosso investimento inicial, podemos ler e escrever código mais rapidamente e com menos bugs.

A razão para isso é simples. Uma definição de função recursiva de cauda tem o mesmo problema como um loop em uma linguagem imperativa: é completamente geral. Ele pode realizar alguns filtragem, mapeamento de alguns, ou quem sabe mais o quê. Somos obrigados a olhar em detalhe toda a definição da função para ver o que ele está realmente fazendo. Em contraste, `map` e funções de manipulação mais outra lista fazer apenas _uma_ coisa. Podemos tomar como garantido que estes blocos de construção simples fazer, e focar na idéia de que o código é tentar expressar, não os mínimos detalhes de como é a manipulação de seus insumos.

No meio do caminho entre a cauda funções recursivas (com a generalidade completo) e nossa caixa de ferramentas de funções de manipulação de lista (cada um deles faz uma coisa) encontram-se as dobras. Uma dobra exige mais esforço para entender que, digamos, uma composição de `map` e `filter`que faz a mesma coisa, mas ele se comporta de forma mais regular e previsível do que uma função recursiva de cauda. Como regra geral, não use uma dobra se você pode compor algumas funções da biblioteca, mas caso contrário tenta usar uma dobra de preferência à mão-rolados uma loop uma recursiva cauda.

Para as funções anônimas, eles tendem a interromper o “fluxo” de ler um pedaço de código. É muitas vezes tão fácil de escrever uma definição de função local em um cláusula `let` ou `where`, e usar isso, como é para colocar uma função anônima em seu lugar. As vantagens relativas de uma função chamada são dois: não precisamos entender a definição da função quando estamos lendo o código que usa-lo, e um nome de função bem escolhido age como um pequeno pedaço de documentação local.

### Space leaks e avaliação rigorosa
-
A função `foldl` que discutimos anteriormente não é o único lugar onde podem ocorrer vazamentos espaço no código Haskell. Vamos usá-lo para ilustrar como a avaliação não-estrita às vezes pode ser problemático, e como resolver as dificuldades que podem surgir.

![[Tip]](support/figs/tip.png)

>Você precisa saber de tudo isso agora?

>É perfeitamente razoável para pular esta seção até que você encontrar um space leak “in the wild”. Desde que você usa `foldr` se você estiver gerando uma lista, e `foldl'` em vez de `foldl` contrário, vazamentos de espaço não são susceptíveis de incomodá-lo na prática por um tempo.

#### Evitar space leaks com seq

Nós nos referimos a uma expressão que não é avaliada preguiçosamente tão _rigorosa_, tão `foldl'` é uma rigorosa deixou desistir. Ele ignora avaliação usual Haskell não-estrita através da utilização de uma função chamada `seq`.

```haskell
-- file: ch04/Fold.hs
foldl' _    zero []     = zero
foldl' step zero (x:xs) =
    let new = step zero x
    in  new `seq` foldl' step new xs
```

Esta função `seq` tem um tipo peculiar, insinuando que ele não está jogando com as regras habituais.

    ghci> :type seq
    seq :: a -> t -> t

Ele funciona da seguinte forma: quando uma expressão `seq` é avaliada seguintes, ele força o seu primeiro argumento a ser avaliada, em seguida, retorna seu segundo argumento. Na verdade, não fazer nada com o primeiro argumento: `seq` existe apenas como uma maneira de forçar que o valor a ser avaliada. Vamos caminhar através de uma aplicação breve para ver o que acontece.

```haskell
-- file: ch04/Fold.hs
foldl' (+) 1 (2:[])
```

Isso expande o seguinte.

```haskell
-- file: ch04/Fold.hs
let new = 1 + 2
in new `seq` foldl' (+) new []
```


O uso de `seq` avalia forçada `novo` a `3`, e retorna seu segundo argumento.

```haskell
-- file: ch04/Fold.hs
foldl' (+) 3 []
```

Acabamos com o resultado seguinte.

```haskell
-- file: ch04/Fold.hs
3
```

Graças a `seq`, não há thunks à vista.

#### Aprender a usar o seq

Sem algum sentido, existe um elemento de mistério para usar efetivamente seguintes. Aqui estão algumas regras úteis para usá-lo bem.

Para ter algum efeito, uma expressão `seq` devem ser a primeira coisa avaliada em uma expressão.

```haskell
-- file: ch04/Fold.hs
-- incorrect: seq is hidden by the application of someFunc
-- since someFunc will be evaluated first, seq may occur too late
hiddenInside x y = someFunc (x `seq` y)
```

```haskell
-- incorrect: a variation of the above mistake
hiddenByLet x y z = let a = x `seq` someFunc y
                    in anotherFunc a z
```

```haskell
-- correct: seq will be evaluated first, forcing evaluation of x
onTheOutside x y = x `seq` someFunc y
```

Para estritamente avaliar vários valores, aplicações da cadeia de `seq` juntos.

```haskell
-- file: ch04/Fold.hs
chained x y z = x `seq` y `seq` someFunc z
```

Um erro comum é tentar utilizar `seq` com duas expressões independentes.

```haskell
-- file: ch04/Fold.hs
badExpression step zero (x:xs) =
    seq (step zero x)
        (badExpression step (step zero x) xs)
```

Aqui, a intenção aparente é o de avaliar estrita `step zero x`. Uma vez que a expressão é repetido no corpo da função, estritamente avaliar a primeira instância de que não terá nenhum efeito sobre o segundo. A utilização de `let` partir da definição de acima `foldl'` mostra como conseguir este efeito corretamente.

Ao avaliar uma expressão, `seq` pára logo que se chega a um construtor. Para os tipos simples, como números, isso significa que irá avaliá-los completamente. tipos de dados algébricos são uma história diferente. Considere o valor `(1+2):(3+4):[]`. Se aplicarmos `seq` para isso, vai avaliar o thunk `(1+2)`. Uma vez que ele irá parar quando atingir o primeiro construtor `(:)`, ele não terá nenhum efeito sobre a conversão segundo. O mesmo é verdadeiro para tuplas: `seq ((1+2),(3+4)) True` não fará nada para o thunks dentro do par, uma vez que imediatamente bate construtor do par.

Se necessário, podemos utilizar técnicas habituais de programação funcional para contornar essas limitações.

```haskell
-- file: ch04/Fold.hs
strictPair (a,b) = a `seq` b `seq` (a,b)

strictList (x:xs) = x `seq` x : strictList xs
strictList []     = []
```

É importante compreender que a `seq` não é livre: ele tem que executar uma verificação em tempo de execução para ver se uma expressão foi avaliada. Use com moderação. Por exemplo, enquanto a nossa função `parEstrito` avalia o conteúdo de um par até o primeiro construtor, ele adiciona as despesas gerais da correspondência padrão, duas aplicações de `seq`, e da construção de uma nova tupla. Se fôssemos medir o seu desempenho no circuito interno de um referência, podemos encontrá-lo para tornar o programa lento.

Além do seu custo de desempenho se em demasia, `seq` não é um milagre cura para todos os problemas de consumo de memória. Só porque você _pode_ avaliar algo estritamente não significa que você _deve_. O uso descuidado do `seq` podem fazer nada; vazamentos mover espaço existente ao redor, ou introduzir novos vazamentos.

Os melhores guias para se `seq` é necessário, e como ele funciona, são medidas de desempenho e perfil, a qual será abordada no [Chapter 25, _Profiling e ajuste de desempenho_](profiling-and-optimization.html "Chapter 25, Profiling e ajuste de desempenho"). A partir de uma base de medição empírica, você irá desenvolver um senso de confiança de quando `seq` é mais útil.

  

* * *

\[[8](#id591518)\] Infelizmente, não temos espaço para abordar esse desafio neste livro.

\[[9](#id594951)\] A barra invertida foi escolhido por sua semelhança visual com a letra grega lambda, `λ`. Embora GHC pode aceitar a entrada Unicode, ele trata corretamente `λ` como uma letra, não como sinônimo de `\`.

![](support/figs/rss.png) Quer ficar atualizado? Assine o feed comentário para [este capítulo](http://book.realworldhaskell.org/feeds/comments/fp/), ou o [livro inteiro](http://book.realworldhaskell.org/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart e John Goerzen. Esta obra está licenciada sob uma [Creative Commons Attribution-Noncommercial 3.0 License](http://creativecommons.org/licenses/by-nc/3.0/). Ícones por [Paul Davey](mailto:mattahan@gmail.com) aka [Mattahan](http://mattahan.deviantart.com/). Tradução por Doralice de Araujo Chaves, Sergio Souza Costa, Nick Rudnick e [Google Translate](http://www.google.com/).


