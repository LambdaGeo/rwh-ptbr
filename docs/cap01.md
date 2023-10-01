[Sumário](index)

## Capítulo 1. Começando



Enquanto lê os primeiros capítulos deste livro, tenha em mente que iremos as vezes apresentar alguns conceitos de modo restrito e simplificado. Haskell é uma linguagem ampla, e apresentar todos os aspectos de um determinado assunto de uma vez só pode atrapalhar o seu aprendizado. Como queremos construir uma base sólida em Haskell, iremos expandir estas explicações iniciais mais a frente.

### Seu ambiente Haskell

Haskell é uma linguagem com muitas implementações, dos quais dois estão em ampla utilização. Hugs é um intérpretador que é usado principalmente para o ensino. Para aplicações reais, o Glasgow Haskell Compiler (GHC) é muito mais popular. Comparado com Hugs, GHCé mais adequado para o “trabalho real”: ele compila para código nativo, suporta a execução paralela e fornece ferramentas de análise de desempenho e de depuração. Por estas razões, GHC é a implementação Haskell que iremos utilizar ao longo deste livro.

GHC tem três componentes principais.

*   **ghc** é um compilador otimizado que gera código nativo rápido.
*   **ghci** é um intérpretador interativo e depurador.
*   **runghc** é um programa para a execução de programas Haskell como scripts, sem a necessidade de compilá-los em primeiro lugar.


>**NOTA**

>Como nos referimos aos componentes do GHC
>
>Quando discutimos o sistema GHC como um todo, vamos nos referir a ele como GHC. Quando precisarmos falar de um comando específico, vamos falar ghc, ghci ou runghc pelo nome.



Neste livro, vamos supor que você está usando pelo menos a versão 6.12.2 do GHC, que foi lançado em 2010. Muitos dos nossos exemplos irá funcionar sem modificações em versões mais antigas. No entanto, _recomendamos_ usar a versão mais recente disponível para sua plataforma. Se você estiver usando Windows ou Mac OS X, você pode começar rápidamente e facilmente usando um instalador pré-construído. Para obter uma cópia do GHC para essas plataformas, visite [a página de download do GHC](http://www.haskell.org/ghc/download.html), e olhe na lista de pacotes binários e instaladores.

Muitas distribuições Linux, BSD Unix e outras variantes, tem disponível pacotes binários personalizado do GHC. Como estes pacotes são construídos especificamente para cada ambiente, eles são muito mais fáceis de instalar e de usar do que os pacotes binários genéricos que estão disponíveis na página de download do GHC. Você pode encontrar uma lista de distribuições de custom-build GHC na página GHC [pacotes de distribuição](http://www.haskell.org/ghc/distribution_packages.html).

Para obter informações mais detalhadas sobre como instalar o GHC em uma variedade de plataformas populares, nós fornecemos algumas instruções no [Apêndice A, _Instalação de GHC e bibliotecas Haskell_](installing-ghc-and-haskell-libraries.html "Apêndice;Instalação de GHC e bibliotecas Haskell").

Começando com ghci, o intérpretador
-----------------------------------

O interpretador interativo para GHC é um programa chamado **ghci**. Nele nós podemos entrar e avaliar expressões Haskell, explorara os módulos, e depurar o nosso código. Se você estiver familiarizado com o Python e Ruby, **ghci**é algo semelhante ao `python` e `irb`, os intérpretadores interativos do Python e Ruby.

O comando ghci tem um foco estreito

Nós normalmente não podemos copiar algum arquivo código de fonte Haskell e colá-lo em **ghci**. Isto não tem um efeito significativo na depuração pedaços de código, mas pode inicialmente ser surpreendente se você está acostumado, por exemplo, o interpretador Python interativo.

Em sistemas Unix-like, rodamos ghci como um comando em uma janela shell. No Windows, está disponível através do Menu Iniciar. Por exemplo, se você instalou usando o instalador do GHC no Windows XP, você deve ir para “Todos programas”, depois “GHC”; então você vai ver ghci na lista. (Veja a seção chamada "Windows".)

Quando rodamos ghci, ele exibe um banner de inicialização, seguido de um prompt Prelude>. Aqui, estamos mostrando a versão 6.12.2 em uma máquina Linux.
```
$ ghci
GHCi, version 6.12.2: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Loading package ffi-1.0 ... linking ... done.
Prelude>
```
A palavra `Prelude` no prompt indica que `Prelude`, uma biblioteca padrão de funções úteis, está carregada e pronto para uso. Quando carregar outros módulos ou arquivos de origem, eles vão aparecer no prompt, também.


>**Obtendo ajuda**
>
>Se você digitar `:?` no prompt **ghci** ele irá imprimir uma detalhada mensagem de ajuda.



O módulo `Prelude` é muitas vezes referida como “the standard prelude”, porque seu conteúdo é definido pelo padrão Haskell 98. Normalmente, ele é simplesmente reduzido à “o prelude”.



>**Sobre o prompt ghci**

>O prompt exibido pelo ghci muda freqüentemente, dependendo de quais módulos temos carregados. Que muitas vezes pode crescer o suficiente para deixar poco espaço visual e uma única linha para a nossa entrada

>Por questões de brevidade e coerência, ao longo deste livro, temos substituído o prompt padrão ghci pelo seguinte prompt `ghci>`

>Se você quiser fazer isso, use a directiva `:set prompt` de ghci, como se segue.

```
Prelude :set prompt "ghci"
ghci>
```


O prelude está sempre implicitamente disponível, nós não precisamos de tomar quaisquer medidas para utilizar os tipos, valores ou funções que ele define. Para utilizar as definições de outros módulos, devemos carregá-los em ghci, utilizando o :module ou :m.

```
ghci> :m +Data.Ratio
```
Agora podemos usar as funcionalidades do módulo `Data.Ratio`, que nos permite trabalhar com números racionais (frações)

### Interação básica: usando ghci como uma calculadora


Além de fornecer uma interface conveniente para testar fragmentos de código, ghci pode funcionar como uma calculadora de desktop facilmente acessível. Podemos facilmente exprimir qualquer operação na calculadora ghci e, como bônus, podemos acrescentar operações mais complexas quando nós nos tornamos mais familiarizado com Haskell. Mesmo utilizando o intérprete desta forma simples, ele pode ajudar-nos a tornar-se mais confortáveis com o modo que o Haskell funciona.

#### Aritméticas simples

Podemos começar imediatamente a entrar expressões, para ver o que ghci vai fazer com eles. Aritmética simples funciona de forma semelhante a linguagens como C e Python: nós escrevemos expressões na forma infixa onde o operador aparece entre os seus operandos.

```
ghci> 2 + 2
4
ghci> 31337 * 101
3165037
ghci> 7.0 / 2.0
3.5
```

O estilo de escrever um infixo expressão é apenas uma conveniência: também podemos escrever uma expressão em forma de prefixo, onde o operador precede os seus argumentos. Para fazer isso, devemos colocar o operador em parênteses.

```
ghci> 2 + 2
4
ghci> (+) 2 2
4
```

Como as expressões acima implicam, ao Haskell ter uma noção de números inteiros e de ponto flutuante. Inteiros podem ser arbitrariamente grande. Aqui, (^) fornece exponenciação inteiro.

```
ghci> 313 ^ 15
27112218957718876716220410905036741257
```

#### Um equívoco aritmético: escrever números negativos

Haskell nos apresenta uma peculiaridade no modo como devemos escrever números: é muitas vezes necessário colocar um número negativo entre parênteses. Isto afeta-nos logo que ir além da simples expressões.

Vamos começar por escrever um número negativo.
```
ghci> -3
-3
```
O - acima, é um operador unário. Em outras palavras, não escrevemos o número único “-3”; nós escrevemos o número “3”, e aplicado o operador - para ele. O operador - é apenas um operador unário do Haskell, e não podemos misturá-la com os operadores infixo.
```
ghci> 2 + -3

<interactive>:1:0:
    precedence parsing error
        cannot mix `(+)' [infixl 6] and prefix `-' [infixl 6] in the same infix expression
```
Se quisermos usar o menos unário perto de um operador infixo, devemos envolver a expressão que se aplica a entre parênteses.
```
ghci> 2 + (-3)
-1
ghci> 3 + (-(13 * 37))
-478
```
Isso evita uma ambigüidade análise. Quando se aplica uma função em Haskell, nós escrevemos o nome da função, seguido do argumento, por exemplo, `f 3`. Se não tivéssemos necessidade de envolver um número negativo entre parênteses, teríamos duas diferentes maneiras de ler profundamente `f-3`: poderia ser “aplicada a função `f` para o número `-3`”, ou “subtrair o número `3` de `f`”

Na maioria das vezes, podemos omitir o espaço em branco (“blank” caracteres, como espaço e guia) de expressões e Haskell irá analisá-los à medida que se destina. Mas nem sempre. Aqui está uma expressão que funciona:
```
ghci> 2*3
6
```
E aqui está um caso que parece similar ao exemplo problemático de número negativo acima, mas a mensagem de erro resultante é diferente.
```
ghci> 2*-3

<interactive>:1:1: Not in scope: `*-'
```
Aqui, a aplicação Haskell está lendo *- como um único operador. Haskell nos permite definir novos operadores (um assunto que voltaremos mais tarde), mas não temos definido *-. Mais uma vez, uns poucos parênteses nos permite resolver este problema.
```
ghci> 2*(-3)
-6
```
Em comparação com outras linguagens, este tratamento incomum de números negativos pode parecer chato, mas representa um trade-off fundamentado. Haskell nos permite definir operadores novos a qualquer momento. O que não é um tipo de recurso de comun a linguagens, vamos ver bastante operadores definidos pelo usuário nos próximos capítulos. Os projetistas da linguagem decidiram aceitar uma sintaxe um pouco pesado para números negativos em troca deste poder expressivo

#### Lógica Booleana, operadores e comparações de valores

Os valores da lógica booleana em Haskell são `True` e `False`. A capitalização destes nomes é importante. A linguagem foi influenciada por C na definição dos operadores para valores booleanos: `(&&)` é lógico “e”, e `(||)` é lógico.
```
ghci> True && False
False
ghci> False || True
True
```
Embora algumas linguagens de programação trate o número zero como sinônimo de `False`, Haskell não, nem considera que um valor diferente de zero deve ser `True`.

```
ghci> True && 1

<interactive>:1:8:
    No instance for (Num Bool)
      arising from the literal `1' at <interactive>:1:8
    Possible fix: add an instance declaration for (Num Bool)
    In the second argument of `(&&)', namely `1'
    In the expression: True && 1
    In the definition of `it': it = True && 1

```
Mais uma vez, somos confrontados com uma mensagem de erro substancial. Em resumo, diz-nos que o tipo Boolean, Bool, não é um membro da família de tipos numéricos, `Num`. A mensagem de erro é bastante longa, pois **ghci** está a apontar a localização do problema, e sugerindo uma possível mudança que nós poderíamos fazer de modo que possa resolver o problema.

Aqui está uma divisão mais detalhada da mensagem de erro.

*   “`No instance for (Num Bool)`” diz-nos que **ghci** está tentando tratar o valor numérico 1 como tendo um tipo Bool, mas não conseguiu.
    
*   “``arising from the literal `1'``” indica que foi o nosso uso do número `1` que causou o problema.
    
*   “``In the definition of `it'``” se refere a uma abreviação **ghci** que iremos rever em algumas páginas a frente.
    

>**Permaneça sem medo diante das mensagens de erro**

>Temos uma importante consideração a fazer aqui, que vamos repetir em todo o trecho inicial do livro. Se você tiver problemas ou mensagens de erro que você ainda não entender, não se desespere. No início, tudo que você precisa fazer é descobrir o suficiente para progredir em um problema. Como você adquirir experiência, será mais fácil de entender as partes das mensagens de erro que inicialmente parece obscuro.

>As várias mensagens de erro têm uma finalidade: eles realmente nos ajudar a escrever o código correto, fazendo-nos executar uma certa quantidade de depuração “a frente”, antes que nós executarmos um programa. Se você estiver vindo de um background de trabalho com linguagens mais permissivas, essa forma de trabalho pode vir como uma espécie de choque.

A maioria dos operadores de comparação Haskell são similares àqueles usados em C e muitas linguagens que foram influencidas
```
ghci> 1 == 1
True
ghci> 2 < 3
True
ghci> 4 >= 3.99
True
```
Um operador que difere de C é o “não é igual”. Em C, este é escrito como !=. Em Haskell, escrevemos (/=), que se assemelha a notação ≠ usada em matemática.
```
ghci> 2 /= 3
True
```
Além disso, onde linguagens similares C costumam usar ! para a negação lógica, Haskell usa a função not.
```
ghci> not True
False
```
#### Precedência de operadores e associatividade

Como na álgebra e em outras linguagens de programação que usam operadores infixo, Haskell tem uma noção de precedência de operadores. Podemos usar parênteses para explicitamente agrupar partes de uma expressão, e a precedência nos permite omitir alguns parênteses. Por exemplo, o operador de multiplicação tem precedência maior do que o operador de adição, de modo que Haskell trata as seguinte expressões como equivalentes.
```
ghci> 1 + (4 * 4)
17
ghci> 1 + 4 * 4
17
```
Haskell atribui valores numéricos a precedência dos operadores, sendo 1 a menor precedência e 9 a maior. Um operador de prioridade maior é aplicada antes de um operador de prioridade inferior. Podemos usar ghci para inspecionar os níveis de precedência de operadores individuais, utilizando o seu comando :info ou :i.
```
ghci> :info (+)
class (Eq a, Show a) => Num a where
  (+) :: a -> a -> a
  ...
  	-- Defined in GHC.Num
infixl 6 +
ghci> :info (*)
class (Eq a, Show a) => Num a where
  ...
  (*) :: a -> a -> a
  ...
  	-- Defined in GHC.Num
infixl 7 *
```
A informação que nós buscamos é na linha “infixl 6 +”, que indica que o (+) operador tem uma precedência de 6. (Nós vamos explicar a saída de outros em um capítulo posterior.) O “infixl 7 *” diz-nos que o (*) operador tem uma precedência de 7. Desde (*) tem uma precedência maior do que (+), podemos ver agora porque 1 + 4 * 4 é avaliado como 1 + (4 * 4), e não (1 + 4) * 4.

Haskell também define associatividade dos operadores. Isso determina como uma expressão contendo múltiplos usos de um operador será avaliad. Se da esquerda para a direita ou para a direita para a esquerda. Os operadores (+) e (*) ficam associativos esquerdos, que é representado como infixl infixl na saída ghci acima. Um operador de direito associativo é exibida com infixr.
```
ghci> :info (^)
(^) :: (Num a, Integral b) => a -> b -> a 	-- Defined in GHC.Real
infixr 8 ^
```
A combinação de regras de precedência e associatividade são geralmente referidos como as fixity rules.

#### Valores indefinidos, e introduzindo variáveis

O prelude de Haskell, a biblioteca padrão mencionado anteriormente, define pelo menos um conhecido constante matemática para nós.
```
ghci> pi
3.141592653589793
```
Mas a sua abrangência de constantes matemáticas não é abrangente, como podemos ver rapidamente. Olhemos para o número de Euler, e
```
ghci> e

<interactive>:1:0: Not in scope: `e'
```
Tudo bem. Nós temos que defini-lo nós mesmos.

>**Não se preocupe com a mensagem de erro**

>Se a mensagem de erro “not in scope” acima parece um pouco assustador, não se preocupe. Tudo isto significa é que não há nenhuma variável definida com o nome e.

Usando a construção let de ghci, nós podemos fazer uma definição temporária e de nós mesmos.
```
ghci> let e = exp 1
```
Esta é uma aplicação da função exponencial, exp, e nosso primeiro exemplo de aplicação de uma função em Haskell. Enquanto linguagens como Python exigem parênteses os argumentos para uma função, Haskell não.

Com `e` definido, agora podemos usá-lo em expressões aritméticas. O operador (^) de exponenciação que introduzimos anteriormente só pode elevar um número a uma potência inteira. Para usar um número de ponto flutuante como o expoente, usamos o operador de exponenciação (**).
```
ghci> (e ** pi) - pi
19.99909997918947
```
>**Essa sintaxe é específica do ghci**

>A sintaxe para let que o ghci aceita não é o mesmo que iremos usar no alto nível "de um programa Haskell normal. Vamos ver a sintaxe normal na [seção chamada “Introduzir variáveis locais”]()

#### Lidar com as regras de precedência e associatividade

Às vezes é melhor deixar pelo menos alguns parênteses no lugar, mesmo quando Haskell permite omitir-los. Sua presença pode ajudar os futuros leitores (incluindo nós mesmos) para entender o que se destina.

Ainda mais importante, expressões complexas que confiam totalmente na precedência do operador são fontes notórias de bugs. Um compilador e um ser humano pode facilmente acabar com noções diferentes do que até mesmo um parêntese, livre expressão curta é suposto fazer.

Não há necessidade de lembrar todas as regras de precedência e associatividade números: é mais simples de adicionar parênteses se tiver dúvidas.

### Edição de linha de comando em ghci


Na maioria dos sistemas, **ghci** tem uma certa quantidade capacidade de edição de linha de comando. Caso você não esteja familiarizado com edição de linha de comando, é uma enorme economia de tempo. Os princípios básicos são comuns a ambos Unix-like e Windows. Pressionando a tecla de seta para **cima** no seu teclado recorda a última linha de entrada que você entrou; pressionando **cima** repetidamente ciclos através de linhas antes da entrada. Você pode usar as teclas **esquerda** e **direita** seta para se movimentar dentro de uma linha de entrada. Em Unix (mas não no Windows, infelizmente), a tecla de **tabulação** completa os identificadores parcialmente inseridos.

>**Onde procurar mais informações**

>Nós mal arranhamos a superfície de edição de linha de comando aqui. Desde que você pode trabalhar com mais eficiência se você estiver mais familiarizado com as capacidades do seu sistema de edição de linha de comando, você pode achar útil fazer algumas leituras complementares.

>Em sistemas Unix-like, ghci usa a [biblioteca GNU readline](http://tiswww.case.edu/php/chet/readline/rltop.html#Documentation) , que é poderoso e personalizável. No Windows, a capacidade do ghci de edição de linha de comando são fornecidas pelo [comando **doskey**](http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/doskey.mspx).

### Listas

A lista é definida entre colchetes, os elementos são separados por vírgulas:
```
ghci> [1, 2, 3]
[1,2,3] 
```
>**Vírgulas são separadores, não terminadores**

>Algumas linguagens permitem o último elemento de uma lista seja seguido por uma vírgula à direita opcional antes de um parêntese de fecho, mas Haskell não permite isso. Se você deixar uma vírgula no final (por exemplo, \[1,2,\]), ), você receberá um erro de análise

A lista pode ser de qualquer tamanho. Uma lista vazia é escrita \[\]
```
 ghci> []
[]
ghci> ["foo", "bar", "baz", "quux", "fnord", "xyzzy"]
["foo","bar","baz","quux","fnord","xyzzy"]
```
Todos os elementos de uma lista devem ser do mesmo tipo. Aqui, violamos esta regra: a nossa lista começa com dois valores Bool mas termina com um string.
```
ghci> [True, False, "testing"]

<interactive>:1:14:
    Couldn't match expected type `Bool' against inferred type `[Char]'
      Expected type: Bool
      Inferred type: [Char]
    In the expression: "testing"
    In the expression: [True, False, "testing"]
```
Mais uma vez, é uma mensagem de erro **ghci** detalhada, mas ela simplesmente diz que não há maneira de transformar a string em um valor booleano, então a expressão lista não está corretamente digitado.

Se nós escrevemos uma série de elementos usando a notação de enumeração, Haskell irá preencher o conteúdo da lista para nós.
```
ghci> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```
Aqui, os caracteres .. denotam uma enumeração. Só podemos usar esta notação para os tipos cujos elementos podemos enumerar. Não faz sentido para cadeias de texto. Por exemplo: `["foo".."quux"].`

De qualquer modo, observe que a utilização acima de notação intervalo nos dá um intervalo fechado; a lista inclui o primeiro e último valor.

Quando escrevemos uma enumeração, podemos, opcionalmente, especificar o tamanho do passo para utilização, fornecendo os primeiros dois elementos, seguido pelo valor em que parar de gerar a enumeração.
```
ghci> [1.0,1.25..2.0]
[1.0,1.25,1.5,1.75,2.0]
ghci> [1,4..15]
[1,4,7,10,13]
ghci> [10,9..1]
[10,9,8,7,6,5,4,3,2,1]
```
Neste último caso acima, a lista é quase sensata faltando o ponto final da contagem, porque não é um elemento da série que nós definimos.

Nós podemos omitir o ponto final de uma enumeração. Se um tipo não tem um natural “limite superior”, isso vai produzir valores indefinidamente. Por exemplo, se você digitar \[1..\] no prompt ghci você terá que interromper ou matar ghci para parar de imprimir uma sucessão infinita de números cada vez maiores. Se você está tentado a fazer isso, tecle Ctrl-C para interromper a contagem. Nós vamos encontrar mais tarde que as listas infinitas são frequentemente útil no Haskell.

>**Cuidado ao enumerar números de ponto flutuante Aqui está um pouco não-intuitivo**
```
ghci> [1.0..1.8]
[1.0,2.0]
```
>Nos bastidores, para evitar problemas de arredondamento em ponto flutuante, o Haskell implementa a enumeração de `1.0` a `1.8+0.5. 12`.

>Usando a notação de enumeração sobre números de ponto flutuante pode pegar mais algumas surpresas, por isso, se você usá-lo, seja cuidadoso. Comportamento de ponto flutuante é peculiar em todas as linguagens de programação, não há nada exclusivo para Haskell aqui.

#### Operadores em listas

Existem dois operadores onipresente para trabalhar com listas. Nós concatenamos duas listas com o operador `(++)`.
```
ghci> [3,1,3] ++ [3,7]
[3,1,3,3,7]
ghci> [] ++ [False,True] ++ [True]
[False,True,True] 
```
O mais básico operador é o (:), que acrescenta um elemento para a frente de uma lista. Este é pronunciado como “cons” (abreviação de “construção”).
```
ghci> 1 : [2,3]
[1,2,3]
ghci> 1 : []
[1]
```
Observe que ao tentar a seguinte expressão \[`1,2]:3` para adicionar um elemento ao final de uma lista, mas o ghci irá rejeitar-lo com uma mensagem de erro, porque o primeiro argumento `(:)` deve ser um elemento, e o segundo deve ser uma lista.

### Strings e caracteres

Se você conhece alguma linguagem de programação, como Perl ou C, você vai achar a anotação Haskell para string familiar.

Uma cadeia de caracteres é delimitada por aspas duplas.
```
ghci> "This is a string."
"This is a string."
```

Como em muitas linguagens, podemos representar caracteres hard-to-see através de “escaping” delas. Escapes in Haskell e as regras de escaping segue as convenções utilizadas e amplamente estabelecida pela linguagem C. Por exemplo, `'\n'` denota um caractere de nova linha, e `'\t'` é um caracter de tabulação. Para detalhes completos, consulte [Apêndice B, _Caracteres, strings, e regras escapando_](characters-strings-and-escaping-rules.html "Apêndice B. Caracteres, strings, e regras escapando")
```
ghci> putStrLn "Here's a newline -->\n<-- See?"
Here's a newline -->
<-- See?
```
O função putStrLn imprime uma string.

Haskell faz uma distinção entre um único caracter e cadeias de caracteres. Um único caractere é colocado entre aspas simples.
```
ghci> 'a'
'a'
```
De fato, uma cadeia de texto é simplesmente uma lista de caracteres individuais. Aqui está uma maneira dolorosa para escrever uma string, que ghci devolve-nos de uma forma mais familiar.
```
ghci> let a = ['l', 'o', 't', 's', ' ', 'o', 'f', ' ', 'w', 'o', 'r', 'k']
ghci> a
"lots of work"
ghci> a == "lots of work"
True
```
A cadeia vazia é escrito "", e é um sinônimo para \[\].
```
ghci> "" == []
True
```
Desde uma string é uma lista de caracteres, podemos utilizar os operadores de lista regulares para a construção de novas cadeias.
```
ghci> 'a':"bc"
"abc"
ghci> "foo" ++ "bar"
"foobar"
```
### Primeiros passos com os tipos


Enquanto nós ja falamos um pouco sobre os tipos, nossas interações com ghci têm sido até agora livre de pensamento do tipo. Nós não disse que tipos em ghci que nós vimos utilizando, e é na sua maioria, dispostos a aceitar a nossa entrada.

Haskell exige que nomes de tipo inicie com uma letra maiúscula e nomes de variáveis devem começar com uma letra minúscula. Tenha isso em mente sobre como você lê, que torna muito mais fácil seguir os nomes.

A primeira coisa que podemos fazer para começar a explorar o mundo de tipos é pedir ao **ghci** para nos dizer mais sobre o que ele é usandondo um comando **ghci**, `:set` ou `:s,` que nos permite alterar alguns dos seus comportamentos padrão. Podemos pedir que imprima mais informações sobre tipo, como a seguir.
```
ghci> :set +t
ghci> 'c'
'c'
it :: Char
ghci> "foo"
"foo"
it :: [Char]
```
O que `+t` faz é dizer ghci para imprimir o tipo de expressão após a expressão. O enigmático `it` na saída pode ser muito útil: ele é realmente o nome de uma variável especial, que armazena no **ghci** o resultado da última expressão avaliada. (Esta não é uma característica da linguagem Haskell, é específico para **ghci**.) Vamos quebrar o significado da última linha de saída **ghci**.

*   Ele está nos dizendo sobre a variável especial `it`.
    
*   Podemos ler o texto da forma `x :: y` no sentido de “a expressão `x` com o tipo `y`”.
    
*   Aqui, a expressão “it” tem o tipo \[Char\]. (O nome String é frequentemente utilizado em vez de \[Char\]. É simplesmente um sinônimo para \[Char\].)




>![[Tip]](/assets/tip.png) O beleza do “it”**

> A variavél `it` é um prático atalho do ghci. Ele permite usar o resultado da expressão que acabamos de avaliar em uma nova expressão.
```
ghci> "foo"
"foo"
it :: [Char]
ghci> it ++ "bar"
"foobar"
it :: [Char]
```

>Quando avaliamos uma expressão, o ghci não muda o valor de `it` caso a valoração falhe. Isso nos permite escrever expressões potencialmente inválidas com segurança.

```
ghci> it
"foobar"
it :: [Char]
ghci> it ++ 3

<interactive>:1:6:
    No instance for (Num [Char])
      arising from the literal `3' at <interactive>:1:6
    Possible fix: add an instance declaration for (Num [Char])
    In the second argument of `(++)', namely `3'
    In the expression: it ++ 3
    In the definition of `it': it = it ++ 3
ghci> it
"foobar"
it :: [Char]
ghci> it ++ "baz"
"foobarbaz"
it :: [Char]
```

Quando unimos o uso do `it` com o livre uso das setas do teclado para lembrar e editar a última expressão que digitamos, nós ganhamos uma boa maneira de experimentar interativamente: o custo de erros é muito baixo. Aproveite a oportunidade de cometer erros baratos e abundantes quando estiver explorando a linguagem.

Aqui está mais alguns nomes de tipos, a partir de expressões que já vimos

```
ghci> 7 ^ 80
40536215597144386832065866109016673800875222251012083746192454448001
it :: Integer
```
Inteiros em haskell tem o nome Integer. O tamanho de um valor inteiro é limitado apenas pela memória do seu computador. 

Os números racionais não aparentam ser os mesmos que os inteiros. Para construir um número racional, nós usamos o operador `(%)`. O numerador vai no lado esquerdo, enquanto o denominador no lado direito.  

```
ghci> :m +Data.Ratio
ghci> 11 % 29
11%29
it :: Ratio Integer
```
Para conveniência, o ghci nos permite abreviar muitos comandos, então podemos escrever :m ao invés de :module para carregar um módulo.

Note as duas palavras no lado direito dos caracteres `::` acima. Nós podemos ler isso como "razão de inteiros". Nós podemos supor que a razão deve conter valores do tipo inteiro no numerador e denominador. Com certeza, se tentarmos construir uma razão onde o numerador e o denominador são de diferentes tipos ou de tipos não inteiros o ghci reclama.

```
ghci> 3.14 % 8

<interactive>:1:0:
    Ambiguous type variable `t' in the constraints:
      `Integral t' arising from a use of `%' at <interactive>:1:0-7
      `Fractional t'
        arising from the literal `3.14' at <interactive>:1:0-3
    Probable fix: add a type signature that fixes these type variable(s)
ghci> 1.2 % 3.4

<interactive>:1:0:
    Ambiguous type variable `t' in the constraints:
      `Integral t' arising from a use of `%' at <interactive>:1:0-8
      `Fractional t'
        arising from the literal `3.4' at <interactive>:1:6-8
    Probable fix: add a type signature that fixes these type variable(s)
```

Embora seja, inicialmente, útil ter o comando **`:set +t`** nos dando informações de todas as expressões que inserimos, este é um recurso que iremos superar rapidamente. Depois de um tempo, frequentemente iremos saber que tipo esperamos que uma expressão tenha. Nós podemos desativar estas informações extras a qualquer momento, usando o comando **`:unset`**.

```
ghci> :unset +t
ghci> 2
2
```

Mesmo com este recurso desabilitado, ainda podemos ter informações facilmente quando precisarmos, usando outro comando.

```

ghci> :type 'a'
'a' :: Char
ghci> "foo"
"foo"
ghci> :type it
it :: [Char]
```

O comando **:type** irá printar informações sobre o tipo de qualquer  expressão que digitarmos (incluindo `it`, como vimos acima). Na verdade, ele não valora a expressão, apenas checa o seu tipo e o imprime. 

Por que os tipos relatados para essas duas expressões são diferentes?

```
ghci> 3 + 2
5
ghci> :type it
it :: Integer
ghci> :type 3 + 2
3 + 2 :: (Num t) => t
```
Haskell tem vários tipos numéricos. Por exemplo, um número literal como `1` pode (dependendo do contexto em que ele aparece) ser um inteiro ou um ponto flutuante. Quando nós forçamos o ghci a valorar a expressão `3 + 2`, ele  precisa escolher um tipo para poder imprimir o valor da expressão, e por padrão é inteiro. No segundo caso, nós pedimos ao ghci para imprimir o tipo da expressão sem valora-la, portanto, não precisa ser tão específico. A resposta, na verdade, é "seu tipo é numérico". Nós iremos ver mais sobre esta notação de tipos em [Capítulo 6. _Usando Classes de tipos_](cap06.md "Usando Classes de tipos")

### Um simples programa

Vamos da um salto a frente e escrever um pequeno programa que conta o número de linhas do seu input. Não espere entender ele ainda, mas é divertido por a mão na massa. Em um editor de texto, insira o seguinte código em um arquivo e o salve como `WC.hs`.

```haskell
-- file: ch01/WC.hs
-- linhas que iniciam com "--" são comentários 

main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"
```
Procure ou crie um arquivo de texto; vamos chamá-lo de `quux.txt`\[[1](#ftn.id577349)\].

```
$ cat quux.txt
Teignmouth, England
Paris, France
Ulm, Germany
Auxerre, France
Brunswick, Germany
Beaumont-en-Auge, France
Ryazan, Russia
```
Em um shell ou linha de comandos, insira o seguinte comando:

```
$ runghc WC < quux.txt
7
```
Nós escrevemos com sucesso um simples programa que interage com o mundo real. Nos capítulos seguintes, nós iremos remover suas dificuldades até que você possa escrever seus próprios programas 

Exercícios
---------

**1.**

Insira as seguintes expressões no ghci. Quais são seus tipos?

*   `5 + 8`
    
*   `3 * 5 + 8`
    
*   `2 + 4`
    
*   `(+) 2 4`
    
*   `sqrt 16`
    
*   `succ 6`
    
*   `succ 7`
    
*   `pred 9`
    
*   `pred 8`
    
*   `sin (pi / 2)`
    
*   `truncate pi`
    
*   `round 3.5`
    
*   `round 3.4`
    
*   `floor 3.7`
    
*   `ceiling 3.3`
    

**2.**

No ghci, digite :? para ver uma ajuda. Defina uma variável, como `let x = 1`, então digite `:show bindings`. O que você vê?

**3.**

A função `words` conta o número de palavras em uma string. Modifique o arquivo `WC.hs` para contar o número de palavras em um arquivo.

**4.**

Modifique o exemplo `WC.hs` novamente, para imprimir o número de caracteres em um arquivo.

  

* * *

\[[1](#id577349)\] Incidentally, what do these cities have in common?

![](/support/figs/rss.png) Want to stay up to date? Subscribe to the comment feed for [this chapter](/feeds/comments/), or the [entire book](/feeds/comments/).
    

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen. This work is licensed under a [Creative Commons Attribution-Noncommercial 3.0 License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul Davey](mailto:mattahan@gmail.com) aka [Mattahan](http://mattahan.deviantart.com/).


