
Uma tradução não oficial do livro Real World Haskell 
de Bryan O'Sullivan, Don Stewart, and John Goerzen



## Capítulo 19. Manipulação de Erro

A manipulação de erros é uma dos temas mais importantes e negligenciados pelos programadores, independente da linguagem utilizada. Em Haskell, você vai encontrar dois tipos principais manipulação de erro: puro e exceções.

Quando falamos de manipulação de erro puro, estamos nos referindo a algoritmos que não requer o emprego de _IO monad_. A vantagem do Haskell é que muitas vezes podemos implementar tratamento de erros apenas usando a expressividade do sistema de tipos ao nosso favor. As exceções em Haskell podem ser lançadas em qualquer lugar, mas só pegadas dentro de um _IO monad_.


### Tratamento de erro através do sistema de tipos


Vamos começar nossa discussão sobre tratamento de erros com uma função muito simples. Digamos que queremos fazer a divisão de uma série de números. Temos um numerador constante, mas gostaríamos de variar o denominador. Poderíamos chegar a uma função como esta:

```haskell
-- arquivo: src/Ch19.hs
module Ch19 where
divBy :: Integral a => a -> [a] -> [a]
divBy numerator = map (numerator `div`)
```

Exemplo de utilização da função: **stack ghci**:

```
$stack ghci src/Ch19.hs
Ch19> divBy 50 [1,2,5,8,10]
[50,25,10,6,5]
Ch19> take 5 (divBy 100 [1..])
[100,50,33,25,20]
```

Funciona como esperado: `50 / 1` é `50` , `50 / 2` é `25` , e assim por diante. Este ainda trabalhou com a lista infinita `[1..]`. O que acontece se aparecer um 0 em algum lugar da lista? 

```
*Ch19> divBy 50 [1,2,0,8,10]
[50,25,*** Exception: divide by zero
```
Não é interessante? **ghci** começou exibindo a saída, então, parou com uma exceção quando ele chegou no zero. Essa é a avaliação preguiçosa em funcionamento - calculou os resultados conforme necessário.

Como veremos mais adiante neste capítulo, na ausência de um manipulador de exceção explícito, essa exceção irá travar o programa. Isso obviamente não é desejável, então vamos considerar maneiras melhores de podermos indicar um erro nesta função pura.

### Uso do Maybe

Uma maneira fácil e imediatamente reconhecível de indicar falha é usar `Maybe`. Em vez de apenas retornar uma lista e lançar uma exceção na falha, podemos retornar Nothing se a lista de entrada contivesse um zero em qualquer lugar, ou apenas com os resultados. Aqui está uma implementação de tal algoritmo:

```haskell
-- arquivo: src/Ch19.hs
divBy' :: Integral a => a -> [a] -> Maybe [a]
divBy' _ [] = Just []
divBy' _ (0:_) = Nothing
divBy' numerator (denom:xs) =
    case (divBy' numerator xs) of
      Nothing -> Nothing
      Just results -> Just ((numerator `div` denom) : results)
```

Exemplo de utilização da função:
```
Ch19> divBy 50 [1,2,5,8,10]
Just [50,25,10,6,5]
Ch19> divBy 50 [1,2,0,8,10]
Nothing
```
A função `divBy'` agora usa uma declaração `case` para verificar se a chamada foi bem-sucedido, assim como `divBy'` faz quando chama a si mesmo.

#### Mantendo a avaliação preguiçosa

O uso de `Maybe` é conveniente, mas tem um custo. A função `divBy` já não pode lidar com as listas infinitas como entrada. Como o resultado é `Maybe [a]`, a lista de entrada inteira deve ser examinada antes de podermos ter a certeza que não vamos retornar `Nothing` devido a um zero em algum lugar. Você pode verificar se este for o caso, tentando um dos exemplos anteriores.

Observe que você não pode ver um resultado parcial aqui, ou seja, você _não_ obteve nenhuma saída. Observe que em cada etapa `divBy'` (exceto para o caso de uma lista de entrada vazia ou um zero no início da lista), os resultados de cada elemento subseqüente devem ser conhecidos antes dos resultados do elemento atual poder ser conhecido. Assim, este algoritmo não pode trabalhar com listas infinitas, e também não é muito eficiente em termos de espaço para grandes listas finitas.

Entre tanto, `Maybe` ainda é uma ótima opção. Neste caso particular, não sabemos se vai haver um problema até chegarmos em avaliar a entrada inteira. Às vezes sabemos de um problema na frente, por exemplo: `tail[]` definido em **prelude** produz uma exceção. Nós podemos facilmente poderíamos escrever várias versões do `tail` que não possui esse problema: 

```haskell
-- arquivo: src/Ch19.hs
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs
```

Isso simplesmente retorna `Nothing` se lhe for dada uma lista de entrada vazia ou `Just` com o resultado de qualquer outra coisa. Como temos apenas que garantir que a lista não esteja vazia antes de saber se temos ou não um erro, usar `Maybe` manter a avaliação preguiçosa. Nós podemos testar isso em **ghci** e ver como ele se compara com o regular `tail`: 

```
Ch19> tail [1,2,3,4,5]
[2,3,4,5]
Ch19> safeTail [1,2,3,4,5]
Just [2,3,4,5]
Ch19> tail []
*** Exception: Prelude.tail: empty list
Ch19> safeTail []
Nothing
```

Aqui, podemos ver o nosso `safeTail` executado como esperado. Mas que tal listas infinitas? Não queremos imprimir um número infinito de resultados, portanto podemos testar com `take 5 (tail [1..])` e uma construção semelhante com `safeTail`:

```
Ch19> take 5 (tail [1..])
[2,3,4,5,6]
Ch19> case safeTail [1..] of {Nothing -> Nothing; Just x -> Just (take 5 x)}
Just [2,3,4,5,6]
Ch19> take 5 (tail [])
*** Exception: Prelude.tail: empty list
Ch19> case safeTail [] of {Nothing -> Nothing; Just x -> Just (take 5 x)}
Nothing
```
Aqui você pode ver que tanto o tail e `safeTail` tratadas as listas infinitas apenas multa. Nota-se que fomos capazes de lidar melhor com uma lista de entrada vazia, em vez de lançar uma exceção, nós decidimos retornar `Nothing` nessa situação. Fomos capazes de atingir o tratamento de erros, sem perder a avaliação preguiçosa.

Mas como aplicamos isso ao nosso exemplo divBy? Vamos considerar a situação lá: falha é uma propriedade de uma entrada ruim individual, não da própria lista de entrada. Que tal tornar a falha uma propriedade de um elemento de saída individual, em vez da própria lista de saída? Ou seja, em vez de uma função do tipo `a -> [a] -> Maybe [a]`, em vez disso, teremos `a -> [a] -> [Maybe a]`. Isso trará o benefício de preservar a avaliação preguiçosa, além de determinar exatamente onde estava o problema - ou até mesmo filtrar os resultados do problema, se desejado. Aqui está uma implementação:


```haskell
-- arquivo: src/Ch19.hs
divBy'' :: Integral a => a -> [a] -> [Maybe a]
divBy'' numerator denominators =
    map safeDiv denominators
    where safeDiv 0 = Nothing
          safeDiv x = Just (numerator `div` x)
```
Dê uma olhada nesta função. Voltamos a usar o `map`, o que é bom tanto para a avaliação preguiçosa quanto para a simplicidade. Podemos experimentá-la em ghci e ver que funciona para listas finitas e infinitas
```
Ch19> divBy 50 [1,2,5,8,10]
[Just 50,Just 25,Just 10,Just 6,Just 5]
Ch19> divBy 50 [1,2,0,8,10]
[Just 50,Just 25,Nothing,Just 6,Just 5]
Ch19> take 5 (divBy 100 [1..])
[Just 100,Just 50,Just 33,Just 25,Just 20]
```
Esperamos que você possa tirar dessa discussão o ponto de que há uma distinção entre a entrada não estar bem formada (como no caso do `safeTail`) e a entrada potencialmente contendo alguns dados inválidos, como no caso do `divBy`. Esses dois casos podem muitas vezes justificar um tratamento diferente dos resultados.

#### Uso do Maybe Mônada

De volta à seção chamada "Uso do Maybe", nós tivemos um programa de exemplo chamado `divby''`. Este exemplo não preservou a avaliação preguiçosa, mas retornou um valor do tipo `Maybe [a]`. O mesmo algoritmo exato poderia ser expresso usando um estilo monádico. Para mais informações e informações importantes sobre as mônadas, consulte o Capítulo 14, Monads. Aqui está nosso novo algoritmo de estilo monádico:

```haskell
-- arquivo: src/Ch19.hs
divBy''' :: Integral a => a -> [a] -> Maybe [a]
divBy''' _ [] = return []
divBy''' _ (0:_) = fail "division by zero in divBy"
divBy''' numerator (denom:xs) =
    do next <- divBy''' numerator xs
       return ((numerator `div` denom) : next)
```
A mônada `Maybe`  deixou o código mais agradável. Para uma mônada Maybe, `return` é o mesmo que `Just`, e `fail _ = Nothing` Podemos agora testar este algoritmo com os mesmos testes que usamos contra o divby'' se quisermos:
```
ghci> divBy''' 50 [1,2,5,8,10]
Just [50,25,10,6,5]
ghci> divBy''' 50 [1,2,0,8,10]
Nothing
ghci> divBy''' 100 [1..] 
*** Exception: stack overflow
```

O código que escrevemos na verdade não é específico para a mônada `Maybe` . Simplesmente mudando o tipo, nós podemos fazê-la funcionar para _qualquer_ mônada. Vamos tentar: 



A função `divByGeneric` contém o mesmo código que `divBy` fez antes; somente demos-lhe um tipo mais geral. Também definimos uma função de conveniência `divBy` com um tipo mais específico.

Vamos tentar isso no **ghci**.

    ghci> 

Os dois primeiros exemplos produzem a mesma saída que vimos antes. Desde então `divByGeneric` não tem um tipo de retorno específico, devemos dar ou deixar um intérprete inferir a partir do ambiente. Se não dermos um tipo de retorno específico, o **ghci** infere a mônada IO. Você pode ver isto no terceiro e no quarto exemplos. O IO converte a mônada `fail` não em uma exceção, como você pode ver no quarto exemplo.

O `Control.Monad.Error` módulo `mtl` faz o pacote `Either String` em uma mônada também. Se você utiliza `Either`, você pode obter um resultado puro que preserva a mensagem de erro, assim:

    ghci> 

Isto nos leva para o próximo tópico de discussão: usar `Either` para retornar informações de erro.

### Utilização do Either

O tipo `Either` é semelhante ao tipo Maybe, com uma diferença fundamental: pode carregar dados que pode representar tanto um erro quanto um sucesso. Embora a linguagem não imponha restrições, por convenção, uma função retornando um `Either` usa um valor de retorno `Left` para indicar um erro e `Right` para indicar sucesso. Se isso ajudar você a lembrar, pode pensar em obter a resposta certa (_Right_). Podemos começar com o nosso exemplo da seção anterior usando Maybe e adaptá-lo para trabalhar com o Either:

```haskell
-- arquivo: src/Ch19.hs
eDivBy :: Integral a => a -> [a] -> Either String [a]
eDivBy _ [] = Right []
eDivBy _ (0:_) = Left "divBy: division by 0"
eDivBy numerator (denom:xs) =
    case eDivBy numerator xs of
      Left x -> Left x
      Right results -> Right ((numerator `div` denom) : results)
```

Este código é praticamente idêntico ao código com `Maybe`, substituindo `Right` por `Just`. E `Left` pode ser comparado ao `Nothing`, mas agora pode levar uma mensagem. Vamos checar a saida no **ghci**:

```
ghci> divBy 50 [1,2,5,8,10]
Right [50,25,10,6,5]
ghci> divBy 50 [1,2,0,8,10]
Left "divBy: division by 0" 
```

#### Tipos de dados personalizados para erros

Enquanto uma `String` indica a causa de um erro pode ser útil para os seres humanos abaixo da estrada, é freqüentemente útil usá-la para definir um tipo de erro personalizado que podemos usar na programação para decidir sobre um curso de ação baseado no problema inicial. Por exemplo, digamos que por alguma razão, além de 0, nós também não iremos dividir por 10 ou 20. Nós poderiamos definir um tipo de erro personalizado, assim:

```haskell
-- arquivo: src/Ch19.hs
data DivByError a = DivBy0
                 | ForbiddenDenominator a
                   deriving (Eq, Read, Show)

eDivBy' :: Integral a => a -> [a] -> Either (DivByError a) [a]
eDivBy' _ [] = Right []
eDivBy' _ (0:_) = Left DivBy0
eDivBy' _ (10:_) = Left (ForbiddenDenominator 10)
eDivBy' _ (20:_) = Left (ForbiddenDenominator 20)
eDivBy' numerator (denom:xs) =
    case eDivBy' numerator xs of
      Left x -> Left x
      Right results -> Right ((numerator `div` denom) : results)
```

Agora, no caso de um erro, por meio do comando `Left`, dados podem ser inspecionados para descobrir a causa exata. Ou podem simplesmente ser impressas com `show`, que vai gerar uma idéia razoável do problema também. Aqui está essa função em ação: 

```
ghci> divBy 50 [1,2,5,8]
Right [50,25,10,6]
ghci> divBy 50 [1,2,5,8,10]
Left (ForbiddenDenominator 10)
ghci> divBy 50 [1,2,0,8,10]
Left DivBy0
```


#### Utilização da Mônada Either

No capítulo chamado "Uso da Mônada Maybe", mostramos como usar `Maybe` em uma mônada. `Either` pode ser usado em uma mônada também, mas pode ser um pouco mais complicado. A razão é que `fail` é codificado para aceitar apenas uma `String` como o código de falha, portanto temos que ter uma forma de mapear tal sequência de caracteres em qualquer tipo que usamos para `Left`. Como você viu anteriormente, `Control.Monad.Error` fornece suporte embutido para `Either String a`, que não envolve nenhum mapeamento para o argumento `fail`. Veja como podemos montar o nosso exemplo, para trabalhar com `Either` no estilo monádico:

```haskell
-- arquivo: src/Ch19/EitherMonadico.hs
{-# LANGUAGE FlexibleContexts #-}

module Ch19.EitherMonadico where


import Control.Monad.Except

data DivByError a = DivBy0 
    | ForbiddenDenominator a 
    | OtherDivByError String 
    deriving (Eq, Read, Show)


divBy :: Integral a => a -> [a] -> Either (DivByError a) [a]
divBy = divByGeneric

divByGeneric :: (Integral a,  MonadError (DivByError a) m) =>
                 a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0:_) = throwError DivBy0
divByGeneric _ (10:_) = throwError (ForbiddenDenominator 10)
divByGeneric _ (20:_) = throwError (ForbiddenDenominator 20)
divByGeneric numerator (denom:xs) =
    do next <- divByGeneric numerator xs
       return ((numerator `div` denom) : next)
```


Aqui, precisamos usar a extensão da linguagem `FlexibleContexts`, a fim de fornecer o tipo de assinatura para `divByGeneric`. A função `divBy` funciona exatamente como antes. Para `divByGeneric`, fazemos `divByError` um membro do `Error` de classe, definindo o que acontece quando alguém chama `fail` (o `strMsg` função). Também convertemos `Right` para return e `Left` para `throwError` permitir que o código seja genérico. Foi ainda necessário adicionar mais uma dependencia ao pacote:

```
dependencies:
- base >= 4.7 && < 5
- mtl
```

### Exceções

A manipulação de exceção é encontrada em muitas linguagens de programação, incluindo Haskell. Pode ser útil, porque, quando ocorre um problema, ela pode fornecer uma maneira fácil de manipulá-la, mesmo que tenha ocorrido várias camadas para baixo através de uma cadeia de chamadas de função. Com algumas exceções, não é necessário para verificar o valor de retorno de cada chamada de função para verificar se há erros, e tomar cuidado para produzir um valor de retorno que reflete o erro, como programadores C deve fazer. Em Haskell, graças a mônadas e aos tipos `Either` e `Maybe`, muitas vezes você pode atingir os mesmos efeitos em código puro, sem a necessidade usar tratamento de exceção.

Alguns problemas -principalmente aqueles que envolvem I/ O –envolvem chamadas para trabalhar com exceções.Em Haskell, exceções podem ser lançadas a partir de qualquer local do programa. No entanto, devido à ordem de avaliação especificada, eles só podem ser capturados na mônada IO.  A manipulação de exceção em Haskell não envolve sintaxe especial como faz em Python ou Java.Os mecanismos para capturar e manipular exceções são funções surpresas.

#### Primeiros passos com exceções

No módulo `Control.Exception`, várias funções e tipos relacionados com as exceções são definidas. Há uma `Exception` tipo definido lá, todas as exceções são do tipo `Exception`. Há também funções para captura e tratamento de exceções. Vamos começar por olhar para `try`, que tem tipo `IO a -> IO (Either Exception a)`. Isto envolve uma ação IO com manipulação de exceção. Se uma exceção foi lançada, ele irá retornar um `Left` valor, como exceção, caso contrário, um `Right` de valor com o resultado original. Vamos tentar isso no **ghci**. Iremos acionar uma exceção não tratada:

    ghci> try (evaluate $ 1 `div` 0) :: IO (Either ArithException Int)

Observe que nenhuma exceção foi acionada pelo `let` declarações. Isso é de se esperar devido à avaliação preguiçosa, a divisão por zero não será tentado até que seja exigido pela tentativa de imprimir `x`. Além disso, observe que existem duas linhas de produção de `try (print y)`. A primeira linha foi produzida pela `print`, que exibiu os 5 dígitos no terminal. O segundo foi produzido por **ghci**, que está mostrando que `print y` retorna `()` e não lança uma exceção.

### Preguiça e Tratamento de Exceção

Agora que você sabe como `try` funciona, vamos tentar outra experiência. Digamos que queremos pegar o resultado do `try` para avaliação futura, para que possamos lidar com o resultado da divisão. Talvez faríamos assim:

    ghci> 

O que aconteceu aqui? Vamos tentar remontá-lo juntos, e ilustrar com uma outra tentativa:

    ghci> 

Como antes, atribuindo `undefined` a `z` não foi um problema. A chave para este quebra-cabeça, é à divisão deste quebra-cabeça, encontra-se com a avaliação preguiçosa. Especificamente, ele está com `return`, que não força a avaliação do seu argumento; só enrola-lo. Assim, o resultado de `try (return undefined)` seria `Right undefined`. Agora, o **ghci** exibi este resultado no terminal. Ele vem pela impressão do `"Right"`, mas você não pode imprimir `undefined` (ou o resultado da divisão por zero). Assim, quando você ver a mensagem de exceção, que está vindo do **ghci**, não é o seu programa.

Este é um ponto chave. Vamos pensar sobre o porquê do nosso exemplo anterior trabalhou e este não. Anteriormente, nós tinhamos colocado `print x` dentro do `try`. Imprimir o valor de algo, é claro, necessita que ele seja avaliado, portanto a exceção foi detectada no lugar certo. Mas simplesmente usando `return` não força a avaliação. Para resolver este problema, o `Control.Exception` define a função do módulo `evaluate`. Ele se comporta exatamente como o `return`, mas força o seu argumento a ser avaliado imediatamente. Vamos tentar:

    ghci> 

Não é o que era esperado. Isso funcionou para ambos `undefined` e para o nosso exemplo de divisão por zero.

![[Tip]](imagens/tip.png)

Dica

Lembre-se: sempre que você está tentando pegar as exceções lançadas pelo codigo puro usando `evaluate` ao invés de `return` dentro da sua função que pega a exceção.

### Usando handle

Muitas vezes, você pode desejar executar uma ação se uma parte do código concluir sem uma exceção, e uma ação diferente da outra. Para situações como está, há uma função chamada handle. Está função tem o tipo `(Exception -> IO a) -> IO a -> IO a`. Ou seja, ele tem dois parâmetros: o primeiro é uma função para chamar caso exista uma exceção durante a execução do segundo. Aqui está uma maneira que nós poderíamos usá-lo:

    ghci> 

Desta forma, podemos imprimir uma mensagem elegante se houver um erro nos cálculos. É mais elegante do que ter o um erro de programa com uma divisão por zero nulo, com certeza.

### Manipulação de Exceções Seletiva

Um problema com o exemplo acima é que ele imprime `"Erro ao calcular o resultado"` para _qualquer_ exceção. Pode ter havido uma exceção ou uma exceção de divisão por zero. Por exemplo, pode ter havido um erro exibido no terminal, ou alguma outra exceção que pode ter sido lançado pelo código puro.

Há uma função `handleJust` para essas situações. A função permite especificar um teste para ver se você está interessado em uma exceção dada. Vamas dar uma olhada:

\-\- file: ch19/hj1.hs
import Control.Exception

catchIt :: Exception -> Maybe ()
catchIt (ArithException DivideByZero) = Just ()
catchIt _ = Nothing
 
handler :: () -> IO ()
handler _ = putStrLn "Caught error: divide by zero"
 
safePrint :: Integer -> IO ()
safePrint x = handleJust catchIt handler (print x)

`catchIt` define uma função que decide se estamos ou não interessado em alguma exceção dada. Ele retorna `Just` em caso afirmativo, e `Nothing` caso contrário. Além disso, o valor atribuido para `Just` será passado para o nosso manipulador. Podemos usar agora o `safePrint` aprimorado:

    ghci> 

O módulo `Control.Exception` também apresenta um número de funções podemos usar como parte de teste em `handleJust` para reduzir os tipos de exceção com as quais devemos nos preocupar. Por exemplo, há uma função `arithExceptions` do tipo `Exception -> Maybe ArithException` que escolherá qualquer `ArithException`, mas ignora nenhum outro. Poderiamos usar da seguinte maneira:

\-\- file: ch19/hj2.hs
import Control.Exception
 
handler :: ArithException -> IO ()
handler e = putStrLn $ "Caught arithmetic error: " ++ show e
 
safePrint :: Integer -> IO ()
safePrint x = handleJust arithExceptions handler (print x)

Deste modo, podemos pegar todos os tipos de `ArithException`, mas ainda deixar outras exceção passar, não modificada e capturada. Podemos vê-la funcionar dessa maneira:

    ghci> 

De particular interesse, você pode observar o teste de `ioErrors`, que corresponde à grande classe de execeção I/O-related.

### Exceções I/O

Talvez a maior fonte de exceção em qualquer programa é I/O. Todos os tipos de coisas podem dar errado quando tratando com o mundo exterior: os discos podem estar cheios, as redes podem cair, ou arquivos pode estar vazios quando você espera que eles tenham dados. Em Haskell, uma exceção I/O é como qualquer outra exceção no que pode ser representado pelo tipo de dados de exceção. Por outro lado, pode haver tantos tipos de exceção I/O, que um módulo especial `System.IO.Error` existe para lidar com eles.

`System.IO.Error` define duas funções: `catch` e `try` que, como as suas contrapartes `Control.Exception`, estão sendo usados para lidar com exceções. Ao contrário das funções `Control.Exception`, contudo, estas funções pegam erros de I/O em armadilhas, e irão passar todas as outras exceções que não foram apanhadas. Em Haskell, erros de I/O têm o tipo `IOError`, que é definido como o mesmo `IOException`.

![[Warning]](imagens/warning.png)

Tenha cuidado para os nomes que você usa

Porque tanto o `System.IO.Error` e o `Control.Exception` definiem as funções com o mesmo nome, se importar ambos em seu programa, você receberá uma mensagem de erro sobre uma referência ambígua para uma função. Você pode importar um ou outro módulo `reservado`, ou esconder os símbolos de um módulo no outro.

Note que o `Prelude` exporta o `System.IO.Error` na versão do `cath`, e não na versão fornecida pelo `Control.Exception`. Lembrando que o primeiro só pode pegar erros de I/O, enquanto o último pode capturar todas as exceções. Em outras palavras, o `catch` no `Control.Exception` é quase sempre o que você vai querer, mas _não_ é o que você vai ficar por padrão.

Vamos dar uma olhada em uma abordagem de exceções utilizando o sistema de I/O para o nosso benefício. Volte na seção chamada "Trabalhando com arquivos de Manipulação", onde foi apresentado um programa que utilizou um estilo imperativo para ler linhas por linha e um arquivo. Embora posteriormente demonstrou mais compacto, "Haskelly" maneiras de resolver esse problema, vamos rever esse exemplo aqui. Na função `mainLoop`, tivemos que testar explicitamente se estamos no final do arquivo de entrada antes de cada tentativa de ler uma linha a partir dele. Ao em vez disso, podemos verificar se a tentativa de ler uma linha que resultou em um erro de EOF, assim:

\-\- file: ch19/toupper-impch20.hs
import System.IO
import System.IO.Error
import Data.Char(toUpper)
 
main :: IO ()
main = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh
 
mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do input <- try (hGetLine inh)
       case input of
         Left e -> 
             if isEOFError e
                then return ()
                else ioError e
         Right inpStr ->
             do hPutStrLn outh (map toUpper inpStr)
                mainloop inh outh

Aqui, nós usamos o `System.IO.Error` na versão `try` para verificar se `hGetLine` colocou um `IOError`. Se assim fosse, nós usariamos `isEOFError` (definido no `System.IO.Error`) para ver se a exceção acionada indica que chegamos ao final do arquivo. Se feito, saímos do loop. Se a exceção for outra coisa, nós chamamos `IOError` para re-colocá-lo.

Há muitos testes e maneiras de extrair informações de `IOError` definido no `System.IO.Error`. Nós recomendamos que você consulte a página na referência da biblioteca quando você precisar saber sobre elas.

### Lançamento de Exceções

Até agora, nós falamos em detalhes sobre o tratamento de exceção. Há um outro pedaço do quebra-cabeça: Lançamento de Exceções \[[41](#ftn.id662948)\]. Nos exemplos vitos até agora neste capítulo, o sistema Haskell coloca execções para você. No entanto, é possível colocar qualquer exceção. Nós vamos mostrar como.

Você perceberá que a maioria dessas funções são apresentadas para retornar um valor de um tipo `a` ou `IO a`. Isto significa que a função pode aparecer para retornar um valor de qualquer tipo. Na verdade, por causa dessas funções acionar exceções, nunca "retorna" nada no sentido normal. Estes valores de retorno que você usa essas funções em vários contextos em que vários tipos diferentes são esperados.

Vamos começar nossa turnê de formas à colocar exceções com as funções `Control.Exception`. A função mais genérica é `throw`, o tipo `Exception -> a`. Esta função pode lançar qualquer `Exception`, e pode fazê-lo em um contexto puro. Existe uma função `throwIO` junto com o tipo de `Exception -> IO` que gera uma exceção na mônada `IO`. Ambas as funções exigem uma `Exception` para coloca-lá. Você pode criar uma `Exception` na mão, ou reutilizar uma `Exception` que já foi criada anteriormente.

Há também uma função `IOError`, que é definida de forma idêntica em ambos os `Control.Exception` e `System.IO.Error` com o tipo `IOError -> IO a`. Isto é, usado quando você deseja gerar uma exceção I/O-related arbitrário.

### Exceções Dinâmicas

Aqui iremos fazer o uso de dois módulos pouco utilizado em Haskell: `Data.Dynamic` e `Data.Typeable`. Não vamos entrar em um grande nível de detalhes sobre os módulos aqui, mas vamos mostrar as ferramentas necessárias para elaborar e usar seu próprio tipo de exceção dinâmica.

No capítulo 21, Usando Banco de Dados, você vai ver que a biblioteca HDBC utiliza exceções dinâmicas para indicar erros de SQL atrás das aplicações. Os erros de bancos de dados muitas vezes têm três componentes: um inteiro que representa um código incorreto, um estado, e uma mensagem incorreta. Vamos implementar a nossa própria HDBC `SQLError` aqui neste capítulo. Vamos começar com a estrutura de dados que representa o próprio erro:

\-\- file: ch19/dynexc.hs
{-# LANGUAGE DeriveDataTypeable #-}
 
import Data.Dynamic
import Control.Exception
 
data SqlError = SqlError {seState :: String,
                          seNativeError :: Int,
                          seErrorMsg :: String}
                deriving (Eq, Show, Read, Typeable)

Por derivação do Typeable typeclass, fizemos este tipo de programação disponíveis para tipagem dinâmica. Para que o GHC possa gerar automaticamente uma instância Tyeable, que tivemos que habilitar a extensão da linguagem `DeriveDataTypeable` \[[42](#ftn.id663179)\].

Agora, vamos definir um `catchSql` e um `handleSql` que pode ser usado para capturar uma exceção que é um `SQLError`. Note que o proveito regular `catch` e `handle` trata funções que não podem pegar o nosso `SQLError`, porque não é um tipo de `Exception`.

\-\- file: ch19/dynexc.hs
{\- | Execute a ação de IO dada.

Se ele levantar um 'SqlError', então execute o
manipulador fornecido e retorne o seu valor de regresso.
De outra maneira, prosseguir como normal -}

catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql = catchDyn
 
{\- | Com 'catchSql', com a ordem de argumentos invertido -}
handleSql :: (SqlError -> IO a) -> IO a -> IO a
handleSql = flip catchSql

Essas funções simples invólucro finos em volta do `catchDyn`, que tem o tipo `Typeable exception => IO a -> (exception -> IO a) -> IO a`. Aqui simplesmente restringimos o tipo de disto para que ele pegue apenas exceções SQL.

Normalmente, quando uma exceção é lançada, mas ela não pega em qualquer lugar, o programa irá travar e irá exibir a exceção ao erro padrão. Com uma exceção dinâmica, no entanto, o sistema não sabe como mostrar isso, portanto você vai ver apenas uma mensagem “exceção desconhecida inútil". Podemos fornecer um utilidade para que os programadores de aplicados possam simplesmente dizer `main = handleSqlError $ = do ...`, e ter certeza de que qualquer exceção lançada (naquele fio) seja exibida. Veja como escrever o `handleSqlError`:

\-\- file: ch19/dynexc.hs
{\- | Os proveitos 'SqlError, e reerguer como os erros de IO com falha.
Útil se você não gosta de pegar erros SQL, mas quer ver uma mensagem
incorreta acontecendo. Muitas vezes usaria isto em alto nível
em volta de chamadas de SQL. -}

handleSqlError :: IO a -> IO a
handleSqlError action =
    catchSql action handler
    where handler e = fail ("SQL error: " ++ show e)

Finalmente, vamos dar um exemplo de como colocar um `SQLError` como uma exceção. Aqui está uma função que fará exatamente isso:

\-\- file: ch19/dynexc.hs
throwSqlError :: String -> Int -> String -> a
throwSqlError state nativeerror errormsg =
    throwDyn (SqlError state nativeerror errormsg)
 
throwSqlErrorIO :: String -> Int -> String -> IO a
throwSqlErrorIO state nativeerror errormsg =
    evaluate (throwSqlError state nativeerror errormsg)

![[Tip]](imagens/tip.png)

Dica

Como um lembrete, `evaluate` é como `return` mas as forças de valiação do seu argumento.

Isso conclui o nosso suporte de exceção dinâmica. Apresentamos uma porção de códigos, e você pode não ter utilizados, mas nós queriamos dar-lhe um exemplo da própria exceção dinâmica e as utilidades que muitas vezes está embutido nela. De fato, os exemplos refletem quase exatamento o que está presente na biblioteca HDBC. Vamos ver um pouco isso no **ghci**:

    ghci> 

A partir daí, você pode ver que **ghci** não sabe como exibir um erro de SQL por si só. No entanto, você também pode ver que nossa função `handleSqlError` ajudou com isso, mas também passou por outros erros não modificados. Vamos finalmente experimentar um manipulador personalizado:

    ghci> 

Aqui, nós definimos um manipulador de erro personalizado com uma nova restrição, que consiste na mensagem no campo `seErrorMsg` do `SQLError`. Percebemos que ela funcionou como o previsto.

Exercícios
----------

1.  Leve o `Either` por exemplo e faça o trabalho de preguiça no estilo do `Maybe`.

Manutenção de erros em mônadas
------------------------------

Porque temos de capturar exceções na mônada IO, se tentar usá-los dentro de uma mônada, ou em uma pilha de transformadores mônada, sairemos para fora da IO da mônada. Isso quase nunca é o que nós realmente queremos.

Nós definimos um transformador MaybeT na seção chamada "Entendendo transformadores mônada através de sua contrução", mas é mais útil como um auxílio para a compreensão do que uma ferramenta de programação. Felizmente, um dedicado e muito mais útil do transformador-mônada já existe: ErrorT, que é definido no módulo `Control.Monad.Error`.

O transformador ErrorT nos permite adicionar restrições a uma mônada, mas ele usa um mecanismo especial, separado no módulo `Control.Exception`. Isso nos da algumas idéias interessantes.

*   Se continuarmos com as interfaces ErrorT, podemos tanto rodar quanto capturar restrições dentro desta mônada.
    
*   Seguindo o padrão de nomeação de outros transformadores mônada, a função de execução é chamado `runErrorT`. Uma restrição não capturada ErrorT vai parar de propagação quando atingir `runErrorT`. Nós não vamos vamos sair em relação a IO da mônada.
    
*   Nós controlaremos os tipos de restrições que tivermos.
    

![[Note]](imagens/note.png)

Não confunda ErrorT com restrições regulares

Se formos usar a função `throw` da `Control.Exception` encontrada dentro da ErrorT (`error` ou `undefined`), que ainda será devolvido até a IO da mônada.

Tal como acontece com outras mônadas `mtl`, a interface que fornece ErrorT é definido por um typeclass.

\-\- file: ch19/MonadError.hs
class (Monad m) => MonadError e m | m -> e where
    throwError :: e             -- error to throw
               -\> m a
 
    catchError :: m a           -- action to execute
               -\> (e -> m a)    -- error handler
               -\> m a

A variável tipo `e` representa o tipo de erro que pretendemos utilizar. Independente do nosso tipo de erro, devemos instanciá-lo da typeclass Error.

\-\- file: ch19/MonadError.hs
class Error a where
    \-\- create an exception with no message
    noMsg  :: a
 
    \-\- create an exception with a message
    strMsg :: String -> a

A função `strMsg` é usada pela aplicação ErrorT de `fail`. Ela lança `strMsg` como uma exceção, passando o argumento de seqüência que recebeu. Quanto à `noMsg`, ela é usada para fornecer uma implementação mzero para o typeclass MonadPlus.

Para apoiar as funções `strMsg` e `noMsg`, o nosso tipo parseError terá um construtor `Chatty`. Este será usado como o construtor se, por exemplo, alguma chamada `fail` em nossa mônada.

Uma ultima parte na qual temos que saber é o tipo da função de execução runErrorT.

    ghci> 

### Um pequeno quadro de análise

Para ilustrar o uso de ErrorT, vamos desenvolver o esqueleto de uma biblioteca de análise semelhante ao Parsec.

\-\- file: ch19/ParseInt.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
 
import Control.Monad.Error
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
 
data ParseError = NumericOverflow
                | EndOfInput
                | Chatty String
                  deriving (Eq, Ord, Show)
 
instance Error ParseError where
    noMsg  = Chatty "oh noes!"
    strMsg = Chatty

Para o estado do nosso analisador gramatical, criaremos uma pilha de transformador de mônada muito pequena. Uma mônada estatal realiza em todo o ByteString a analise, e empilha no topo ErrorT para fornecer tratamento de erros.

\-\- file: ch19/ParseInt.hs
newtype Parser a = P {
      runP :: ErrorT ParseError (State B.ByteString) a
    } deriving (Monad, MonadError ParseError)

Como de costume, nós envolvemos a nossa mônada pilha em um `Newtype`. Isto não nos custa nada na realização, mas acrescenta a segurança do tipo. Evitamos deliberadamente conseguir um exemplo de MonadState B.ByteString. Isto significa que os usuários da mônada Parser não serão capaz de utilizar, `get` ou `put` para consulta ou modificar o estado do analisador gramatical. Por conseguinte, forçamo-nos a fazer um levantamento manual para alcançar a mônada estatal na nossa pilha. Isto é, contudo, muito fácil fazer.

\-\- file: ch19/ParseInt.hs
liftP :: State B.ByteString a -> Parser a
liftP m = P (lift m)
 
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- liftP get
  case B.uncons s of
    Nothing         -> throwError EndOfInput
    Just (c, s')
        | p c       -> liftP (put s') >> return c
        | otherwise -> throwError (Chatty "satisfy failed")

A função `catchError` é útil para tarefas além da simples manipulação de erro. Por exemplo, podemos facilmente “modificar” uma restrição, convertendo-a em uma forma mais amigável e simples.

\-\- file: ch19/ParseInt.hs
optional :: Parser a -> Parser (Maybe a)
optional p = (Just \`liftM\` p) \`catchError\` \\_ -> return Nothing

A nossa função de execução simplesmente “esconde” em conjunto várias camadas, e reajusta o resultado em uma forma mais organizada.

\-\- file: ch19/ParseInt.hs
runParser :: Parser a -> B.ByteString
          -\> Either ParseError (a, B.ByteString)
runParser p bs = case runState (runErrorT (runP p)) bs of
                   (Left err, _) -> Left err
                   (Right r, bs) -> Right (r, bs)

Se carregarmos isso em **ghci**, podemos ver seus passos.

    ghci> 

### Exercícios

**1.**

Escreva `many` Parses, com o tipo Parser a -> Parser \[a\]. Deve aplicar-se um analisador até falhar.

**2.**

Use `many` Parses para escrever um `int` Parses, com o tipo Parses Int.Tanto pode aceitar números negativos quanto positivos.

**3.**

Modifique seu `int` parser para lançar uma exceção `NumericOverflow`, veja se ele descobrirá um excesso numérico na analise.

  

* * *
https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling

