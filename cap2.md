---
# You don't need to edit this file, it's empty on purpose.
# Edit theme's home layout instead if you wanna make some changes
# See: https://jekyllrb.com/docs/themes/#overriding-theme-defaults
layout: page
---
Uma tradução não oficial do livro Real World Haskell 
de Bryan O'Sullivan, Don Stewart, and John Goerzen

-------------------------------------------------------
## Capítulo 2. Tipos e Funções           

### Por que se preocupar com tipos?

Cada expressão e função em Haskell tem um _tipo_. Por exemplo, o valor `True` tem o tipo Bool, enquanto o valor `"foo"` tem o tipo String. O tipo de um valor indica que ele compartilha certas propriedades com outros valores do mesmo tipo. Por exemplo, podemos adicionar os números, e podemos concatenar as listas, que são propriedades desses tipos. Dizemos que uma expressão “tem tipo `X`”, ou “é do tipo `X`”.

Antes de lançar-se uma discussão mais aprofundada do sistema de tipos do Haskell, vamos falar sobre o motivo pelo qual devemos nos preocupar com tipos: o que são _e para que servem_? No nível mais baixo, um computador está preocupado com bytes, com quase nenhuma estrutura adicional. O que o sistema de tipos nos dá é a _abstração_. Um tipo acrescenta significado de bytes simples: ela nos permite dizer que “estes bytes são texto”, “os bytes são uma reserva de linha aérea”, e assim por diante. Normalmente, um sistema de tipo vai além do que isso nos impede de mistura acidental de tipos acima: por exemplo, um sistema de tipo geralmente não nos deixa tratar uma reserva de hotel como se fosse um carro de recibo de aluguel.

O benefício da introdução de abstração é que ela nos deixa esquecer ou ignorar os detalhes de baixo nível. Se eu sei que um valor no meu programa é uma seqüência, eu não tenho que saber os detalhes íntimos de como as strings são implementadas: eu só posso supor que a minha string vai se comportar como todos os outros textos que eu já usei.

O que faz do sistemas de tipo interessante é que eles não são todos iguais. Na verdade, sistemas de tipo diferentes frequentemente não estão sequer preocupado com os mesmos tipos de problemas. O sistema de tipos de uma linguagem de programação aprofunda a nossa forma de pensar, e escrever o código, em que a linguagem.

O sistema de tipos do Haskell nos permite pensar em um nível muito abstrato: ela nos permite escrever  progrmas concisos e poderosos.

O sistema de tipo do Haskell
----------------------------

Há três aspectos interessantes sobre tipos em Haskell: eles são _fortes_, eles são _estáticos_, e podem ser automaticamente _inferidos_. Vamos falar mais detalhadamente sobre cada uma dessas idéias. Quando possível, nós vamos apresentar semelhanças entre os conceitos do tipo de sistema de Haskell e idéias relacionadas em outras linguagens. Iremos também abordar os pontos fortes e fracos de cada uma dessas propriedades.

### Tipos fortes

Quando dizemos que Haskell tem um sistema do tipo _forte_, queremos dizer que o sistema garante que um tipo de programa não pode incluir certos tipos de erros. Esses erros vêm surgem da tentativa de escrever expressões que não fazem sentido, como a utilização de um número inteiro como uma função. Por exemplo, se uma função espera para trabalhar com números inteiros, e passar-lhe uma string, o compilador Haskell irá rejeitá-lo.

Chamamos uma expressão que obedece a regras dos tipos da linguagem como _bem tipada_. Uma expressão que desobedece as regras de tipo é _mal tipado_, e causará um _erro de tipo_.

Outro aspecto da visão do Haskell de tipagem forte é que ela não irá automaticamente coagir (fazer uma coertção) dos valores de um tipo para outro. (Coerção também é conhecido como o vazamento ou conversão). Por exemplo, um compilador C automaticamente e silenciosamente coagi um valor do tipo int em um float em nosso código, se uma função espera um parâmetro do tipo float, mas um compilador Haskell vai dar um erro de compilação em uma situação similar. Temos que explicitar a coerção dos tipos nas aplicação das funções. Neste caso dizemos que estamos fazendo um cast.  

Tipagem forte pode tornar mais difícil escrevermos certos tipos de código. Por exemplo, uma forma clássica de escrever código de baixo nível na linguagem C, é dado um array de bytes, realizar um cast com ele para tratar os bytes como realmente sendo uma estrutura de dados complexa. Isto é muito eficiente, uma vez que não nos obrigam a copiar os bytes ao redor. O sistema de tipos do Haskell não permite este tipo de coerção. A fim de obter a mesma visão estruturada dos dados, seria preciso fazer algumas cópias, o que custaria um pouco no desempenho.

A grande vantagem da tipagem forte é que ela captura bugs real em nosso código antes que possam causar problemas. Por exemplo, em uma linguagem fortemente tipada, não podemos acidentalmente utilizar uma cadeia onde um inteiro é esperado.

![[Note]](support/figs/note.png)

Os tipos mais fracos e os tipos mais fortes

É útil ter consciência de que muitas comunidades linguísticas têm suas próprias definições de um “tipo forte”. No entanto, vamos falar brevemente e em termos gerais sobre a noção de força nos sistemas do tipo.

Em ciência da computação acadêmica, os significados de “forte” e “fraco” têm um sentido técnico estrito: refere-se à força de _como permissivo_ um sistema de tipo é. Um sistema de tipo mais fraco trata mais expressões como válida do que um sistema de tipo mais forte.

Por exemplo, em Perl, a expressão `"foo" + 2` avalia o número 2, mas a expressão `"13foo" + 2` avalia o número 15. Haskell rejeita ambas as expressões como inválido, porque o `(+)` operador exige tanto de seus operandos ser numéricos. Por causa do tipo de sistema de Perl ser mais permissivo do que Haskell, dizemos que ela é mais fraca nos termos do presente técnica de interpretação restrita.

Todo "barulho" em torno de sistemas do tipo têm suas raízes no  Inglês coloquial, onde as pessoas atribuem noções de _valor_ para as expressões “fraco” e “forte”: geralmente pensamos que força é melhor do que fraqueza. De um lado, programadores que falam mais o Inglês coloquial e do outro os  acadêmicos que usam mais os jargões acadêmicos _jogam pedras_ em qualquer sistema de tipo que não satisfiça as suas expectativas. O resultado é sempre um passatempo popular na Internet, uma verdeira guerra.

### Tipos estáticos

Ter um sistema do tipo _estático_ significa que o compilador sabe o tipo de cada valor e de expressão em tempo de compilação, antes que qualquer código seja executado. Um compilador ou intérpretador Haskell irá detectar quando tentarmos usar expressões cujos tipos não coincidem, e rejeitar o nosso código com uma mensagem de erro antes de executá-lo.

    ghci> 

Esta mensagem de erro é do tipo que já vimos antes. O compilador tem inferido que o tipo da expressão `"false"` é \[Char\]. O `(&&)` exige que cada operador de seus operandos como sendo do tipo Bool, e seu operando à esquerda de fato tem este tipo. Desde que o real tipo de `"false"` não corresponde ao tipo necessário, o compilador rejeita esta expressão como mal escrita.

Tipagem estática pode ocasionalmente dificultar a escrita de alguns tipos de código útil. Em linguagens como Python “duck typing” é comum, quando um objeto age parecido como um outro para ser utilizado como um substituto para ele \[[2](#ftn.id578004)\]. Felizmente, o sistema de _typeclasses_ do Haskell, que iremos cobrir em [Capítulo 6, _Utilizar Typeclasses_](using-typeclasses.html "Capítulo 6. Utilizar Typeclasses"), fornece quase todos os benefícios de tipagem dinâmica, de uma forma segura e conveniente. Haskell tem algum suporte para a programação com tipos dinâmicos reais, embora não seja tão fácil como em um linguagem que inteiramente encorpora à noção de tipos dinâmicos.

A combinação de tipagem forte e estática, torna impossível a ocorrência de erros de tipo em tempo de execução. Enquanto isso significa que irar requerem mais do pensamento “a frente”, ele também elimina muitos erros simples que podem de outra maneira ser extremamente difíceis de encontrar. É um truísmo na comunidade Haskell que uma vez que o código compila, é mais provável que funcione corretamente do que em outras linguagens. (Talvez uma maneira mais realista de colocar isso é que o código Haskell muitas vezes tem menos bugs trivial).

Programas escritos em linguagens com tipagem dinâmica requer grandes conjuntos de testes para dar alguma garantia de que os erros de tipo simples, não pode ocorrer. Conjunto de teste não podem oferecer cobertura completa: algumas tarefas comuns, tais como refactorar um programa para torná-lo mais modular, pode introduzir erros de tipo novo que um conjunto de testes não pode expor.

Em Haskell, o compilador comprova a ausência de erros de tipo para nós: um programa que compila Haskell não sofrem de erros tipo quando ele é executado. Refactoring é geralmente uma questão de se mover em torno do código, em seguida, recompilar e arrumar algumas vezes até que o compilador dá-nos a “tudo limpo”.

Uma analogia útil para entender o valor de tipagem estática é olhar para ela como a colocação de peças em um quebra-cabeças. Em Haskell, se uma parte tem a forma errada, ela simplesmente não vai caber. Numa linguagem com tipagem dinâmica, todas as peças são quadrados 1x1 e sempre em forma, então você tem que analisar constantemente a imagem resultante e verificar (através de testes) se é correta.

### Inferência de tipo

Finalmente, um compilador Haskell pode automaticamente deduzir os tipos de quase\[[3](#ftn.id578076)\] todas as expressões de um programa. Esse processo é conhecido como _inferência de tipo_. Haskell permite declarar explicitamente o tipo de qualquer valor, mas a presença de inferência de tipo significa que este é quase sempre opcional, não algo que somos obrigados a fazer.

O que esperar do sistema tipo
-----------------------------

Nossa exploração dos recursos e benefícios importantes do sistema de tipo no Haskell terá a duração de uma série de capítulos. No início, você pode achar uma tarefa lidar com tipos em Haskell.

Por exemplo, em vez de simplesmente escrever algum código e executá-lo para ver se ele funciona como se poderia esperar em Python ou Ruby, primeiro você precisa ter certeza de que seu programa passa pelo controle de verificação de tipo. Por que alongar a curva de aprendizagem?

While tipagem forte e estática torna o Haskell seguro, inferência de tipos o torna conciso. O resultado é potente: vamos acabar com uma linguagem que é tanto mais seguro do que linguagens populares estaticamente tipada, e muitas vezes mais expressiva do que linguagens dinamicamente tipadas. Esta é uma afirmação forte a fazer, e vamos apoiá-lo com provas ao longo do livro.

Corrigindo os erros de tipo podemos inicialmente sentir mais trabalho do que se estivessemos usando uma linguagem dinâmica. Pode ajudar ao olhar isso como verificar depuração _a frente_. O compilador mostra a você muitas das falhas lógicas em seu código, em vez de deixá-lo tropeçar em problemas durante a execução.

Além disso, dado que Haskell pode inferir os tipos de suas expressões e funções, você ganha os benefícios de tipagem estáticas _sem_ o fardo de “ficar tipando”. Em outras linguagens, o sistema de tipo atende às necessidades do compilador. Em Haskell, ele _serve_ você. A desvantagem é que você tem de aprender a trabalhar no âmbito que ele proporciona.

Vamos introduzir novos usos de tipos Haskell ao longo deste livro, para nos ajudar a escrever e testar códigos práticos. Como resultado, o entendimento completo do "por que o sistema de tipo" vale a pena surgirão gradualmente.  

Alguns tipos básicos comuns
---------------------------

Na [seção intitulada “Primeiros passos com os tipos”](getting-started.html#starting.types "Primeiros passos com os tipos"), apresentamos alguns tipos. Aqui estão mais alguns de tipos básicos mais comuns.

*   Um valor Char representa um caractere Unicode.
    
*   Um valor Bool representa um valor na lógica booleana. Os valores possíveis do tipo Bool são `True` e `False`.
    
*   O tipo Int é usada para inteiros com sinal e tamnho fixo. A faixa exata de valores representável como Int depende do sistema do inteiro “nativo”: em uma Máquina de 32 bits, um Int é geralmente 32 bits de largura, enquanto a-máquina de 64 bits, normalmente é 64 bits de largura. O padrão Haskell apenas garante que um Int é maior do que 28 bits. (Existem tipos numéricos que são exatamente 8, 16, e assim por diante bits, assinado em formatos com e sem sinal; nós vamos chegar nelas mais tarde.)
    
*   Um valor Integer é um inteiro de tamanho ilimitado. Integers não são usadas tão frequentemente como Ints, porque eles são mais caros, tanto em desempenho quanto em consumo do espaço. Por outro lado, os cálculos não são terminados comInteger overflow, então eles dão de forma mais confiável os acertos.
    
*   Os valores de tipo Double são usados para números de ponto flutuante. Um valor Double é normalmente 64 bits de largura, e usa o sistema nativo de ponto flutuante de representação. (O tipo Float, também existe, mas seu uso não é recomendado; o compilador Haskell concentra-se em tornar o Double mais eficiente, de modo que o Float é muito mais lento).
    

Já fizemos uma breve visita da notação para os tipos em Haskell [na seção chamada “Primeiros passos com os tipos”](getting-started.html#starting.types "Primeiros passos com os tipos"). Quando escrevemos um tipo explicitamente, usamos a notação de `expression :: MyType` para dizer que a `expression` tem o tipo MyType. Se omitirmos o `::` e o tipo que se segue, o compilador Haskell irá inferir o tipo da expressão.

    ghci> 

A combinação de `::` e depois o tipo é chamado um _assinatura de tipo_.

Aplicação de função
-------------------

Agora que já completamos, por enquanto, algumas informações sobre os tipos de dados. Podemos voltar a nossa atenção de como _trabalhar_ com alguns dos tipos que já estavamos aplicando. Para isso iremos usar algumas funções.

Para aplicar uma função em Haskell, podemos escrever o nome da função seguido por seus argumentos.

    ghci> 

Nós não usamos parênteses ou vírgulas para separar o grupo ou argumentos para uma função, apenas escrevemos o nome da função, seguido por cada argumento em ordem, é o suficiente. Como exemplo, vamos aplicar a função `compare`, que leva dois argumentos.

    ghci> 

Se você está acostumado com a sintaxe de chamada de função em outros linguagens, pode demorar um pouco para se acostumar com esta notação , mas ela é simples e uniforme.

Aplicação de função tem precedência maior do que usando os operadores. Por exemplo, as seguintes expressões têm o mesmo significado.

    ghci> 

Os parênteses acima não fazem mal nenhum, mas adiciona um pouco de ruído visual. Às vezes, porém, _devemos_ usar parênteses para indicar a forma como queremos que uma expressão complicada seja analisada.

    ghci> 

Isto é, aplica-se o `compare` com os resultados da aplicação `sqrt 3` e `sqrt 6`, respectivamente. Se omitir os parênteses, parece que estamos tentando passar quatro argumentos para `compare`, em vez dos dois que ele aceita.

Tipos de dados compostos uteis: listas e tuplas
-----------------------------------------------

Um tipo de dados composto é construído a partir de outros tipos. Os tipos mais comuns de dados compostos em Haskell são listas e tuplas.

Nós já vimos o tipo de lista na [seção chamada “Strings e caracteres”](getting-started.html#starting.string "Strings e caracteres"), onde descobrimos que Haskell representa uma seqüência de texto como uma lista de valores Char, e que o tipo de “lista de Char” é escrito \[Char\].

A função `head` (“cabeça”) retorna o primeiro elemento de uma lista.

    ghci> 

Por outro lado a função, `tail` (“cauda”), retorna todos elementos, _menos_ a cabeça de uma lista.

    ghci> 

Como você pode ver, podemos aplicar `head` e `tail` na lista de tipos diferentes. Aplicando `head` para um valor \[Char\] retorna um valor Char, ao aplicar a um valor \[Bool\] retorna um valor Bool. Para a função `head` não importa o tipo dos elementos da lista.

Como os valores de uma lista podem possuir qualquer tipo, chamamos a lista de tipo _polimórfico_\[[4](#ftn.id578737)\]. Quando queremos escrever um tipo polimórfico, usamos uma _variável de tipo_, que deve começar com uma letra minúscula. Uma variável tipo é um espaço reservado, onde eventualmente vamos substitui-lá por um tipo real.

Podemos escrever o tipo de “lista de `a`” colocando a variável tipo entre colchetes: \[a\]. Isso equivale a dizer: “Eu não me importo com o tipo que eu tenho, posso fazer uma lista com ele”.

![[Note]](support/figs/note.png)

Distinguir nomes de tipo e as variáveis tipo

Agora podemos ver porque um nome do tipo deve começar com uma letra maiúscula: o que torna diferente uma variável do tipo, é que ela deve começar com uma letra minúscula.

Quando falamos que uma lista tem valores de um tipo específico, nós estamos substituindo o tipo pelo nosso tipo. Assim, por exemplo, o tipo \[Int\] é uma lista de valores do tipo Int, pois substituimos `a` por Int. Da mesma forma,  `[MyPersonalType]` é uma lista de valores do tipo `MyPersonalType`. Podemos realizar esta substituição recursivamente, também: `[[Int]]` é uma lista de valores do tipo `[Int]`, ou seja, uma lista de listas de `Int.`

    ghci> 

O tipo da expressão acima é uma lista de listas de Bool.

![[Note]](support/figs/note.png)

As listas são especiais

As listas são o “pão e a manteiga” de coleções Haskell. Em uma linguagem imperativa, poderíamos realizar uma tarefa com muitos itens iteragindo através de um loop. Isso é algo que nós frequentemente , fazemos em Haskell através do percorrimento em uma lista, seja por recursão ou usando uma função que faça a recursão para nós. Listas são a forma mais fácil  que temos para usar dados que estruturem o nosso programa e o seu fluxo de controle. Nós vamos gastar muito mais tempo para discutir as listas no [Capítulo 4, _A programação funcional_](functional-programming.html "Chapter 4. Functional programming").

Uma tupla é uma coleção de tamanho fixo de valores, onde cada valor pode ter um tipo diferente. Isto distingue-os de uma lista, que pode ter qualquer tamanho, mas cujos elementos devem ter todos o mesmo tipo.

Para ajudar a entender a diferença, vamos dizer que queremos controlar dois pedaços de informação sobre um livro. Tem um ano de publicação, que é um número e um título, que é uma seqüência de caracteres. Nós não podemos manter estes dois pedaços de informação em uma lista, porque eles têm tipos diferentes. Em vez disso, usamos uma tupla.

    ghci> 

Nós escrevemos uma tupla colocando seus elementos entre parênteses e separando-os com vírgulas. Nós usamos a mesma notação para escrever seu tipo.

    ghci> 

Há um tipo especial, (), que atua como uma tupla de zero elementos. Esse tipo tem apenas um valor, também por escrito `()`. Tanto o tipo e o valor são normalmente pronunciado “unit”. Se você estiver familiarizado com C, () é similar ao void.

Haskell não tem uma noção de tupla de um elemento. Tuplas são frequentemente designadas de acordo com o número de elementos como um prefixo. A-tupla que tem 2 dois elementos, é normalmente chamado de _pair_. A “3-tupla” (algumas vezes chamado de _triplo_) tem três elementos, um 5-tuplo tem cinco, e assim por diante. Na prática, trabalhar com tuplas que contenham muitos elementos torna o código pesado, de modo tuplas com muitos elementos raramente são utilizados.

Um tipo de uma tupla representa o número, posições e tipos de seus elementos. Isto significa que tuplas contendo diferentes números e tipos de elementos têm tipos distintos, como fazem as tuplas cujos tipos aparecem em ordens diferentes.

    ghci> 

Neste exemplo, a expressão `(False, 'a')` tem o tipo (Bool, Char), que é diferente do tipo de `('a', False)`. Mesmo que o número de elementos e seus tipos são os mesmos, estes dois tipos são distintos, porque as posições dos tipos de elemento são diferentes.

    ghci> 

Este tipo, (Bool, Char, Char), é diferente de (Bool, Char) pois contém três elementos, e não dois.

Muitas vezes usamos tuplas para devolver múltiplos valores de uma função. Nós também podemos usá-los a qualquer momento, quando precisamos de uma coleção de tamanho fixo de valores, se as circunstâncias não exigem um tipo container personalizado.

### Exercícios

**1.**

Quais são os tipos das seguintes expressões?

*   `False`
    
*   `(["foo", "bar"], 'a')`
    
*   `[(True, []), (False, [['a']])]`
    

Funções sobre listas e tuplas
-----------------------------

Nossa discussão de listas e tuplas mencionou como podemos construí-los, mas pouco sobre como podemos trabalhar com eles depois. Nós só introduzimos duas funções de lista até agora, `head` e `tail`.

Um par de funções relacionadas com lista, `take` e `drop`, toma dois argumentos. Dado um número `n` e uma lista, `take` o primeiro retorna `n` elementos da lista, enquanto a `drop` retorna todos, _menos_ os primeiros `n` elementos da lista. (Como estas funções possuem dois argumentos, note que nós separar cada função e seus argumentos utilizando o espaço em branco).

    ghci> 

Para tuplas, as funções `fst` e `snd` retornam o primeiro e segundo elemento do par, respectivamente.

    ghci> 

Se o seu background  for em uma outra linguagem de programação, estes exemploes  podem ser parecidos com uma chamada de uma função de dois argumentos. Nos termos da convenção Haskell para a aplicação da função, cada uma é uma aplicação de uma função a um único parametro, que um par de elementos.

![[Note]](support/figs/note.png)

Tuplas Haskell não são listas imutáveis

Se você está vindo do mundo do Python, você provavelmente tem usado listas e tuplas como conceitos quase intercambiáveis. Embora os elementos de uma tupla Python são imutáveis, que podem ser indexados e iterada usando os mesmos métodos como uma lista. Este não é o caso em Haskell, portanto, não tente levar essa idéia com você em território linguístico desconhecido.

Como exemplo, dê uma olhada no tipo de assinaturas `fst` e `snd`: eles são definidos apenas para os pares, e não pode ser usado com tuplas de outros tamanhos. O sistema de tipos do haskell torna complicado  escrever uma função generalizada que pegue o segundo elemento de qualquer tupla, não importa quão grande seja”.

### Passando uma expressão para uma função

Em Haskell, a aplicação de função é associativa a esquerda. Isto é melhor ilustrado por exemplo: a expressão `a b c d` é equivalente a `(((a b) c) d)`. Se quisermos usar uma expressão com um argumento de uma outra, temos de usar parênteses para dizer explicitamente ao analisador o que realmente significa. Aqui está um exemplo.

    ghci> 

Podemos ler isto como “passe a expressão `drop 4 "azerty"` como argumento de `head`”. Se tivéssemos deixado de fora os parênteses, a expressão seria semelhante a passar três argumentos para `head`. Compilação falhará com um erro do tipo, `head` requer um único argumento, uma lista.

Tipos de função e pureza
------------------------

Vamos dar uma olhada no tipo de uma função.

    ghci> 

Podemos ler o `->` acima como “para”, que se traduz vagamente como “retorna”. A assinatura como um todo,  lê como “`lines` tem o tipo String para lista-de-String”. Vamos tentar aplicar a função.

    ghci> 

A função `lines`divide uma string em limites de linha. Observe que a sua assinatura nos deu uma dica sobre o que a função pode realmente fazer: ela pega uma String, e retorna muitas. Esta é uma valiosa propriedade incrível de tipos em uma linguagem funcional.

Um _efeito colateral_ introduz uma dependência entre o estado global do sistema e o comportamento de uma função. Por exemplo, vamos deixar de lado o Haskell, por um momento e pensar em uma linguagem de programação imperativa. Considere uma função que lê e retorna o valor de uma variável global. Se algum outro código pode modificar a variável global, então o resultado de uma particular chamada de nossa função depende do valor atual da variável global. A função tem um efeito colateral, mesmo que ele nunca modifica a própria variável.

Os efeitos colaterais são essencialmente entradas ou saídas invisíveis as funções. Em Haskell, o padrão é para as funções não terem efeitos colaterais: o resultado de uma função depende apenas das entradas providas explicitamente. Chamamos essas funções de _puras_; funções com efeitos colaterais são _impuras_.

Se uma função tem efeitos colaterais, podemos dizer a partir da leitura de sua assinatura: o tipo do resultado da função começará com IO.

    ghci> 

O sistema de tipo de Haskell nos impede acidentalmente misturar códigos impuro e puro.

Arquivos fonte Haskell, e escrevendo funções simples
----------------------------------------------------

Agora que sabemos como aplicar as funções, é hora de voltamos nossa atenção para como escrevê-las. Podemos escrever funções em **ghci**, entretanto ele não é um bom ambiente para isso. Ele só aceita um subconjunto muito limitado do Haskell: e o mais importante, a sintaxe que é utilizada para a definição de funções não é o mesma quando usamos um arquivo fonte Haskell\[[5](#ftn.id579534)\]. Em vez disso, vamos finalmente mudar a forma que usamos até então e criar um arquivo de fonte.

Arquivos fonte em Haskell geralmente são identificadas com um sufixo `.hs`. Aqui está a definição de uma função simples: crie um arquivo chamado `soma.hs`, e adicionar este conteúdo a ele.

\-\- arquivo: ca03/soma.hs  
soma a b = a + b

No lado esquerdo do `=` temos o nome da função, seguido pelos argumentos da função. Do lado direito temos o corpo da função. Com o nosso arquivo fonte salvo, podemos carregá-lo em **ghci** com **:load** ou **:l**, e usar nosso nova função `soma` imediatamente. (O prompt que é exibido pelo **ghci** vai mudar depois que você carregar o arquivo.)

    ghci> 

![[Note]](support/figs/note.png)

E se ghci não conseguir encontrar o arquivo fonte?

Quando você executar **ghci** pode não ser capaz de encontrar o seu arquivo fonte. Ele irá procurar por arquivos fonte em qualquer diretório que ele foi executado. Se este não é o diretório que seu arquivo fonte esta, na verdade, você pode usar o comando **:cd** do **ghci** para alterar o seu diretório de trabalho.

    ghci> 

Alternativamente, você pode fornecer o caminho para o arquivo fonte Haskell como argumento de **:l**. Esse caminho pode ser absoluto ou relativo à o diretório atual do **ghci**.

Quando aplicamos `soma` aos valores `1` e `2`, as variáveis de `a` e `b` do lado esquerdo de nossa definição é dada (ou “ligada”) aos valores `1` e `2`, assim, o resultado é a expressão `1 + 2`.

Haskell não possui uma palavra-chave de **return**, como uma função é uma expressão simples, e não uma seqüência de instruções. O valor da expressão é o resultado da função. (Haskell possui uma função chamada `return`, mas não vamos discutir isso por um tempo, mas ela tem um significado diferente do que nas linguagens imperativas).

Quando você vê um simbolo `=` no Haskell ele tem o seguinte “sentido”: o nome do lado esquerdo é definido como sendo a expressão do lado direito.

### Apenas o que é uma variável, afinal?

Em Haskell, uma variável fornece uma maneira de dar um nome a uma expressão. Uma vez que uma variável é _vinculada_ (ou seja, associada a) uma expressão particular, o seu valor não muda: sempre podemos usar o nome da variável em vez de escrever a expressão e obter o mesmo resultado de qualquer maneira.

Se você está acostumado a linguagens de programação imperativa, é provável que você vê uma variável como uma forma de identificar uma _locação de memória_ (ou algo equivalente), que pode conter valores diferentes em momentos diferentes. Em uma linguagem imperativa, podemos mudar o valor de uma variável a qualquer momento, de modo que analisar o local da memória várias vezes pode potencialmente dar resultados diferentes cada vez.

A diferença fundamental entre essas duas noções de uma variável é que, em Haskell, uma vez que temos uma variável ligada a uma expressão, nós sabemos que sempre podemos substituí-lo por essa expressão, porque não vai mudar. Em uma linguagem imperativa, esta noção de substituição não se sustenta.

Por exemplo, se executar o seguinte script Python minúsculo, ele irá imprimir o número 11.

x = 10  
x = 11  
\# value of x is now 11  
print x

Em contraste, tentando o equivalente em Haskell resultará em um erro.

\-\- arquivo: ca02/Atribuição.hs  
x = 10  
x = 11

Nós não podemos atribuir um valor para `x` duas vezes.

    ghci> 

### Avaliação condicional

Como muitas outras linguagens, em Haskell temos uma expressão `if` . Vamos vê-la em ação, então iremos explicar o que está acontecendo. Por exemplo, vamos escrever nossa própria versão da função `drop`. Antes de começar, vamos aprofundar um pouco sobre como `drop` de funções. Antes de começar, vamos aprofundar um pouco sobre como o `drop` se comporta, para que possamos reproduzir seu comportamento.

    ghci> 

Do exposto, parece que a `drop` retorna a lista original, se o número a remover é inferior ou igual a zero. Caso contrário, ele remove os elementos até que se esgote ou atingir o número indicado. Aqui está uma função `meuDrop` que tem o mesmo comportamento, e usa a expressão Haskell `if` para decidir o que fazer. A função `null` abaixo verifica se a lista está vazia.

\-\- arquivo: ca02/meuDrop.hs  
meuDrop n xs = if n <= 0 || null xs  
               then xs  
               else meuDrop (n - 1) (tail xs)

Em Haskell, a indentação é importante: ele _continua_ uma definição existente, em vez de começar um novo. Não omita a identação!

Você pode perguntar de onde o nome da variável `xs` vem na função Haskell. Este é um padrão de nomenclatura comum para a lista: você pode ler o `s` como um sufixo, por isso o nome é essencialmente “plural de `x`”.

Vamos salvar nossa função Haskell em um arquivo chamado `meuDrop.hs`, em seguida, carregá-lo em **ghci**.

    ghci> 

Agora que vimos `meuDrop` em ação, vamos voltar ao código fonte e olhar todas as novidades que temos apresentado.

Em primeiro lugar, nós introduzimos `--`, o início de um comentário de uma única linha. Este comentário se estende até o final da linha.

Seguinte é a palavra chave `if` de em si. Ela introduz uma expressão que tem três componentes.

*   Uma expressão do tipo Bool, imediatamente após o `if`. Referimo-nos a isso como um _predicado_.
    
*   Uma palavra-chave `then`, seguido por outra expressão. Esta expressão será utilizada como o valor do `if` se a avaliação da expressão predicado for `True`.
    
*   Uma palavra-chave `else`, seguido por outra expressão. Esta expressão será utilizada quando o valor do `if` se a avaliação da expressão predicado for `False`.
    

Iremos nos referir às expressões após as palavras-chave `then` e `else`  como “ramos”. Os ramos devem ter os mesmos tipos, a expressão `if` terá também seu tipo. Uma expressão como `if True then 1 else "foo"` tem diferentes tipos para seus ramos, por isso ela está errada e será rejeitada por um compilador ou interpretador.

Lembre-se que o Haskell é uma linguagem funcional baseada em expressões. Em uma linguagem imperativa, pode fazer sentido omitir o ramo `else`  de um `if`, porque estamos trabalhando com as _declarações_, não expressões. No entanto, quando estamos trabalhando com expressões, um `if` que está faltando um `else` não teria um resultado ou tipo, se o predicado avaliar para `False`, por isso seria absurdo.

Nosso predicado contém algumas novidades a mais. A função `null` indica se a lista é vazia, enquanto o `(||)` realiza um operador lógico “ou” de seus argumentos do tipo Bool.

    ghci> 

![[Tip]](support/figs/tip.png)

Os operadores não são especiais

Observe que fomos capazes de encontrar o tipo de `(||)` por envolvimento em parênteses. O operador `(||)` não é “incorporado” a linguagem: é uma função comum.

O operador `(||)` é “curto-circuito”: se o operando da esquerda é avaliado como `True`, ela não avalia o operando direito. Na maioria das linguagens, a avaliação de curto-circuito requer um apoio especial, mas não em Haskell. Vamos ver porque em breve.

Em seguida, aplica-se a nossa função de forma recursiva. Este é o nosso primeiro exemplo de recursão, que falaremos em detalhes em breve.

Finalmente, o nosso `if` abrange várias linhas de expressão. Nós alinhamos os ramos `then` e `else`  no âmbito do `if` para clareza. Enquanto nós usamos algum recuo, o valor exato não é importante. Se quisermos, podemos escrever a expressão inteira em uma única linha.

\-\- arquivo: ca02/meuDrop.hs  
meuDropX n xs = if n <= 0 || null xs then xs else meuDropX (n - 1) (tail xs)

O tamanho desta versão o torna mais difícil de ler. Nós geralmente fazemos uma quabra na expressão `if`  através de várias linhas para manter o predicado e cada um dos ramos mais fácil de identificar.

Para comparação, aqui é um equivalente Python da função Haskell `meuDrop`. Os dois são estruturados de forma semelhante: decremento um contador a cada remoção de um elemento da cabeça da lista.

def meuDrop(n, elts):  
    while n > 0 and elts:  
        n = n - 1  
        elts = elts\[1:\]  
    return elts

Compreender a avaliação através de exemplos
-------------------------------------------

Na nossa descrição do `meuDrop`, nós temos até agora focado nas características superfíciais. Precisamos aprofundar e desenvolver um modelo mental útil de como funciona a aplicação de função. Para fazer isso, iremos trabalhar através de alguns simples exemplos, até que possamos caminhar através da avaliação da expressão `meuDrop 2 "abcd"`.

Já falamos várias vezes sobre a substituição de uma expressão para uma variável, e nós vamos fazer uso deste recurso aqui. Nosso procedimento implicará em reescrever expressões repetidas, substituindo as expressões para as variáveis até chegar a um resultado final. Este seria um bom momento para buscar um lápis e papel, de modo que você possa acompanhar nossas descrições, e tenta-lás sozinho.

### Avaliação preguiçosa

Vamos começar olhando para a definição de uma função simples não-recursiva.

\-\- arquivo: ch02/RoundToEven.hs  
isOdd n = mod n 2 == 1

Aqui, `mod` é um função padrão que retorna o resto de uma divisão inteira. O grande primeiro passo para entender como a avaliação funciona em Haskell é descobrir qual será o resultado da avaliação da expressão `isOdd (1 + 2)`.

Antes de explicar como procede a avaliação em Haskell, vamos recapitular o tipo de estratégia de avaliação utilizados por algumas familias de linguagens. Em primeiro lugar, avaliamos a subexpressão `1 + 2`, que dará `3`. Em seguida, aplique a função `odd` com `n` e substituido por `3`. Finalmente, avaliamos `mod 3 2` que dará `1`, e `1 == 1` dando `True`.

Em uma linguagem que utiliza avaliação _estrita_, os argumentos para uma função são avaliados antes da função ser aplicada. Haskell escolhe um outro caminho: avaliação _não-estrita_.

Em Haskell, a subexpressão `1 + 2` _não_ será reduzido para o valor `3`. Em vez disso, criamos uma “promessa” que, quando o valor da expressão `isOdd (1 + 2)` é requerida, nós vamos ser capazes de calcular isso. O registro que nós usamos para controlar a expressão não avaliada é referida como uma _thunk_. Isto é _tudo_ o que acontece: vamos criar um thunk, e adiar a avaliação propriamente dita, até que é realmente sejá necessário. Se o resultado desta expressão nunca for posteriormente utilizado, não vamos calcular o seu valor.

Avaliação não-estrita é muitas vezes referida como a _avaliação preguiçosa_\[[6](#ftn.id580576)\].

### Um exemplo mais amplo  

Vamos agora olhar para a avaliação da expressão `meuDrop 2 "abcd"`, onde usamos o `print` para garantir que ele será avaliado.

    ghci> 

Nosso primeiro passo é tentar aplicar `print`, que necessita que o seu argumento seja avaliado. Para fazer isso, nós aplicamos a função `meuDrop` com os valores `2` e `"abcd"`. Nós ligamos a variável `n` para o valor `2`, e `xs` para `"abcd"`. Se substituirmos esses valores em predicado de `meuDrop`, obtemos a seguinte expressão.

    ghci> 

Em seguida, avaliamos o predicado suficiente para descobrir qual o seu valor. Isso requer que podemos avaliar a expressão `(||)`. Para determinar o seu valor, o operador `(||)` deve examinar o valor de seu operando à esquerda em primeiro lugar.

    ghci> 

Substituindo esse valor para a expressão `(||)`  levará à seguinte expressão.

    ghci> 

Se o operando esquerdo avaliou para `True`, o operador `(||)` não precisaria avaliar seu operando à direita, uma vez que não irá afetar o resultado da expressão. Uma vez que a avaliação der `False`, o operador `(||)` deve avaliar o operando direito.

    ghci> 

Nos agora substituimos este valor de retorno para a expressão `(||)`. Uma vez que ambos os operandos são avaliadas como `False`, a expressão `(||)` tambem, portanto o predicado é avaliado como `False`.

    ghci> 

Isso faz com que ramo `else` da expressão `if` será avaliada. Este ramo contém uma aplicação recursiva da função `meuDrop`.

![[Note]](support/figs/note.png)

Curtos-circuitos de graça

Muitas linguagens necessitam tratar o operador lógico "ou" especialmente para que os curto-circuitos aconteça, se seu operando à esquerda é avaliada como `True`. Em Haskell, `(||)` é uma função comum: a avaliação não-estrita constrói essa capacidade para a linguagem.

Em Haskell, podemos facilmente definir uma nova função que sejá curto-circuito.

\-\- arquivo: ca02/curtoCircuito.hs  
`newOr` a b = if a then a else b

Se escrevermos uma expressão como `newOr` `True (length [1..] > 0)`, não irá avaliar o seu segundo argumento. (Isto é: essa expressão tenta calcular o comprimento de uma lista infinita. Se fosse avaliada, iria travar o **ghci**, executando um loop infinitamente, até que mate o processo.)

Se fôssemos escrever uma função comparável, digamos, Python, avaliação rigorosa complicaria nós: ambos os argumentos serão avaliados antes de serem passados para `newOr`, e nós não seriamos capaz de evitar o loop infinito no segundo argumento.

### Recursão

Quando aplicamos `meuDrop` recursivamente, `n` é obrigado a thunk `2 - 1`, e `xs` a `tail "abcd"`.

Estamos avaliando agora `meuDrop` desde o início novamente. Nós substituímos os novos valores de `n` e `xs` no predicado.

    ghci> 

Aqui está uma versão condensada da avaliação do operando à esquerda.

    ghci> 

Como devemos agora esperar, não avaliamos a expressão `2 - 1` até que nós precisávamos do seu valor. Também avaliamos o operando direito preguiçosamente, adiando `tail "abcd"` até precisarmos  do seu valor.

    ghci> 

O predicado novamente é avaliado como `False`, fazendo com que a clausula `else`  seja avaliada mais uma vez.

Porque nós avaliamos as expressões para `n` e `xs` para avaliar o predicado, agora sabemos que nessa aplicação do `meuDrop`, `n` tem o valor `1` e `xs` tem o valor `"bcd"`.

### Finalizando a recursividade

Na próxima aplicação recursiva de `meuDrop`, ligamos `n` para `1 - 1` a `xs` a `tail "bcd"`.

    ghci> 

Mais uma vez, para o `(||)` é necessário avaliar seu operando à esquerda em primeiro lugar.

    ghci> 

Finalmente, esta expressão foi avaliada como `True`!

    ghci> 

Porque o operando direito não pode afetar o resultado de `(||)`, não é avaliado, eo resultado do predicado é `True`. Isso nos leva a avaliar a clausula `then`.

    ghci> 

### Retornando a recursão

Lembre-se, agora estamos dentro da nossa segunda aplicação recursiva do `meuDrop`. Esta aplicação é avaliada como `tail "bcd"`. Voltamos a partir da aplicação da função, substituindo esta expressão para `meuDrop (1 - 1) (tail "bcd")`, retornando o resultado dessa aplicação.

    ghci> 

Nós então retornamos a partir da primeira aplicação recursiva, substituindo o resultado da segunda aplicação recursiva para `meuDrop (2 - 1) (tail "abcd")`, retornando o resultado dessa aplicação.

    ghci> 

Finalmente, o nosso retorno de aplicação original, substituindo o resultado da primeira aplicação recursiva.

    ghci> 

Observe que como nós retornamos a partir de cada aplicação recursiva sucessiva, nenhum deles necessitou avaliar a expressão `tail "bcd"`: o resultado final da avaliação da expressão original é um _thunk_. O thunk só é avaliado quando finalmente **ghci** precisar imprimi-lo.

    ghci> 

### O que aprendemos?

Nós estabelecemos vários pontos importantes aqui.

*   Faz sentido usar substituição e reescrita para entender a avaliação de uma expressão em Haskell.
    
*   A avaliação preguiçosa leva-nos a adiar a avaliação até termos um valor, e avaliamos apenas o suficiente de uma expressão para determinar o seu valor.
    
*   O resultado da aplicação de uma função pode ser um thunk (uma expressão em diferido).
    

Polimorfismo em Haskell
-----------------------

Quando introduzimoas as listas, mencionamos que o tipo de lista é polimórfico. Vamos falar sobre o polimorfismo Haskell em mais detalhes aqui.

Se quisermos buscar o último elemento de uma lista, usamos a função `last`. O valor que ele retorna deve ter o mesmo tipo que os elementos da lista, mas `last` opera da mesma maneira, não importa qual o tipo desses elementos realmente são.

    ghci> 

Para captar esta idéia, a sua assinatura tipo contém uma "type variable".

    ghci> 

Aqui, `a` é a "type variable". Podemos ler a assinatura como “tem uma lista, da qual todos os elementos têm algum tipo `a`, e retorna um valor do mesmo tipo `a`”.

![[Tip]](support/figs/tip.png)

Identificação de uma variável de tipo

Type variables sempre começam com uma letra minúscula. Você sempre pode dizer uma type variable de uma variável normal pelo contexto, porque as linguagens de tipos e funções são separadas: variáveis tipo existem nas assinaturas, tipo e variáveis normais existem em expressões regulares.

É prática comum em Haskell manter os nomes de variáveis tipo muito curto. Uma carta é esmagadoramente comum; nomes já aparecem com pouca freqüência. Tipo de assinaturas são geralmente breves, ganhamos mais legibilidade, mantendo nomes curtos, tornando-descritivo.

Quando uma função tem "type variable" na sua assinatura, o que indica que alguns dos seus argumentos podem ser de qualquer tipo, chamamos a função polimórfica.

Quando queremos aplicar `last` digamos, uma lista de Char, o compilador substituto Char para cada `a` em todo o tipo de assinatura, o que nos dá o tipo de `last` com uma entrada de \[Char\] como \[Char\] -> Char.

Este tipo de polimorfismo é chamado polimorfismo _paramétrico_ . A escolha do nome é fácil de entender, por analogia: assim como uma função pode ter parâmetros que podemos ligar mais tarde a valores reais, um tipo Haskell podem ter parâmetros que podemos ligar mais tarde para outros tipos.

![[Tip]](support/figs/tip.png)

Um pouco nomenclatura

Se um tipo contém parametros para tipos, nós dizemos que é um tipo parametrizado, ou um tipo polimórfico. Se uma função ou do tipo de valor contém parâmetros de tipo, nós chamamos de polimorfismo.

Quando vemos um tipo parametrizado, nós já observamos que o código não importa qual é o tipo realmente. Contudo, podemos fazer uma declaração mais forte: _não tem jeito de descobrir qual é o tipo real,_ ou para manipular um valor desse tipo. Não se pode criar um valor, nem pode inspeccionar um. Tudo o que podemos fazer é tratá-lo como uma “caixa preta” totalmente abstrata. Nós vamos tratar a razão por que isso é importante em breve.

Polimorfismo paramétrico é o tipo mais visível de polimorfismo que o suporta Haskell. Polimorfismo paramétrico no Haskell diretamente influencia o projeto de recursos genéricos do Java e C#. Um tipo parametrizado em Haskell é semelhante a uma variável do tipo Generics Java. C++ templates também têm uma semelhança com o polimorfismo paramétrico.

Para tornar mais claro como o polimorfismo em Haskell difere de outras linguagens, aqui estão algumas formas de polimorfismo que são comuns em outros linguagens, mas não estão presentes em Haskell.

Normalmente em linguagens orientadas a objeto, o polimorfismo de _subtipo_ é mais amplo do que o polimorfismo paramétrico. Os mecanismos de subclasse de C++ e Java dá-le o polimorfismo subtipo. A classe base define um conjunto de comportamentos que suas subclasses pode modificar e estender. Desde que Haskell não é uma linguagem orientada a objetos, ele não fornecem polimorfismo de subtipo.

Também é comum o polimorfismo _coerção_ o que permite um valor de um tipo a ser convertido implicitamente em um valor de outro tipo. Muitas linguagens fornecem alguma forma de coerção polimorfismo: um exemplo é a conversão automática entre números inteiros e de ponto flutuante. Haskell deliberadamente evita mesmo tipo de coerção automática simples.

Isso não é toda a história do polimorfismo em Haskell: vamos voltar ao assunto no [Capítulo 6, _Usando Typeclasses_](using-typeclasses.html "Chapter 6. Using Typeclasses").

### Raciocínio sobre funções polimórficas

Na [seção denominada “Tipos de função e pureza”](types-and-functions.html#funcstypes.sigs "Function types and purity"), falamos sobre como descobrir o comportamento de uma função com base na sua assinatura. Podemos aplicar o mesmo tipo de raciocínio para funções polimórficas. Vamos dar uma olhada novamente na `fst`.

    ghci> 

Em primeiro lugar, observe que o seu argumento contém duas variáveis do tipo, `a` e `b`, o que significa que os elementos da tupla pode ser de diferentes tipos.

O tipo do resultado de `fst` é `a`. Já mencionamos que o polimorfismo paramétrico faz o tipo real inacessível: `fst` não tem informações suficientes para a construção de um valor do tipo `a`, nem pode transformar um `a` em um `b`. Assim, _único_ comportamento válido possível (omitindo loops infinitos ou falha) que pode ter é retornar o primeiro elemento do par.

#### Outras leituras

Há um profundo sentido matemático em que qualquer função não-patológicos do tipo (a,b) -> a deve fazer exatamente o que `fst` faz. Além disso, essa linha de raciocínio se estende a mais complicada de funções polimórficas. O documento \[[Wadler89](bibliography.html#bib.wadler89 "[Wadler89]")\] abrange este procedimento em profundidade.

_Tem sido sugerido que nós devemos criar “uma caixa de teoria” para as discussões das coisas profundas, e referências a trabalhos acadêmicos._

O tipo de uma função de mais de um argumento
--------------------------------------------

Até agora, nós não vimos muitas assinaturas de funções que têm mais de um argumento. Já tinhamos usados algumas dessas funções, vamos olhar para a assinatura de uma, `take`.

    ghci> 

É muito claro que existe alguma coisa acontecendo com um Int e algumas listas, mas porque há dois símbolos `->` na assinatura? Grupos Haskell, essa cadeia de setas da direita para a esquerda, isto é, `->` é associativo à direita. Se introduzirmos parênteses, podemos definir como a assinatura deste tipo será interpretada.

\-\- arquivo: ca02/Take.hs  
take :: Int -> (\[a\] -> \[a\])

A partir disso, parece que devemos ler a assinatura de tipo como uma função que recebe um argumento, um Int, e retorna outra função. Essa outra função também tem um argumento, uma lista, e retorna uma lista do mesmo tipo, como seu resultado.

Isso é correto, mas não é fácil de ver quais seriam as suas consequências. Voltaremos a este tema na [seção chamada “Aplicação de função parcial e currying”](functional-programming.html#fp.partialapp "Partial function application and currying"), uma vez que passamos um pouco de tempo escrevendo funções. Por agora, podemos tratar do tipo após a última `->` como sendo função do tipo de retorno, e os tipos anteriores como os próprios argumentos da função.

Podemos agora escrever uma assinatura tipo da função `meuDrop` que definimos anteriormente.

\-\- arquivo: ca02/meuDrop.hs  
meuDrop :: Int -> \[a\] -> \[a\]

Exercícios
----------

**1.**

Haskell oferece uma função padrão, `last :: [a] -> a`, que retorna o último elemento de uma lista. Da leitura do tipo sozinho, quais são os possíveis comportamentos válidos (omitindo falhas e loops infinitos) que esta função poderia ter? Que coisas que esta função claramente não pode fazer?

**2.**

Escreva uma função `lastButOne,` que retorna o elemento _antes_ do último.

**3.**

Carregue seu função `lastButOne` no **ghci**, e teste-o em listas de diferentes comprimentos. O que acontece quando você passar uma lista que é muito curta?

Por que a confusão sobre a pureza?
----------------------------------

Poucas linguagens de programação vai tão longe como Haskell, insistindo que a pureza deve ser o padrão. Esta escolha tem consequências profundas e valiosas.

Como o resultado da aplicação de uma função pura só pode depender de seus argumentos, muitas vezes podemos obter um forte indício de que uma função pura faz simplesmente lendo o seu nome e compreensão da sua assinatura. Como exemplo, vamos olhar `not`.

    ghci> 

Mesmo que não saiba o nome desta função, a sua assinatura só limita a validade comportamentos possíveis que poderia ter.

*   Ignore o seu argumento, e sempre quer retornar `True` ou `False`.
    
*   Returna seu argumento não modificado.
    
*   Negue seu argumento.
    

Sabemos também que esta função _não_ pode fazer algumas coisas: não pode acessar os arquivos, não pode falar com a rede e não posso dizer qual é a hora.

Pureza faz o trabalho de compreensão de código mais fácil. O comportamento de uma função pura não depende do valor de uma variável global, ou o conteúdo de um banco de dados, ou o estado de uma conexão de rede. Código puros são inerentemente modular: cada função é independente, e tem uma interface bem definida.

A consequência óbvia de pureza não ser o padrão é que trabalhar com o código _impuro_ se torna mais fácil. Haskell incentiva um estilo de programação onde nós separamos o código que _deverá_ ter efeitos colaterais do código que não precisa deles. Nesse estilo, o código impuro tende a ser simples, com a “heavy lifting” realizada em código puro.

Grande parte do risco em software encontra-se em falar com o mundo exterior, seja lidando com dados incorretos ou faltantes, ou manipulação de ataques maliciosos. Por causa do sistema de tipos do haskell nos dizemos exatamente quais as partes do nosso código tem efeitos colaterais, que podem estar adequadamente sobre a guarda. Porque o nosso estilo de codificação mantém isolado impuro e simples, a nossa “superfície de ataque” é pequena.

Conclusão
---------

Neste capítulo, nós tivemos uma visão geral do turbilhão do sistema de tipos do Haskell e grande parte da sua sintaxe. Nós lemos sobre os tipos mais comuns, e descobrimos como escrever funções simples. Nós introduzimos polimorfismo, expressões condicionais, pureza e sobre avaliação preguiçosa.

Isso tudo equivale a uma grande quantidade de informação para absorver. No [Capítulo 3, _Definir os tipos, racionalizando as funções_](defining-types-streamlining-functions.html "Chapter 3. Defining Types, Streamlining Functions"), vamos construir esse conhecimento básico para melhorar ainda mais a nossa compreensão do Haskell.

  

* * *

\[[2](#id578004)\] “ If it walks like a duck, and quacks like a duck, then let's call it a duck. ” (Se anda como um pato e grasna como um pato, então vamos chamá-lo de um pato.)

\[[3](#id578076)\] Às vezes, precisamos dar ao compilador um pouco de informação para ajudá-lo a fazer uma escolha para a compreensão do nosso código.

\[[4](#id578737)\] Vamos falar mais sobre o polimorfismo na [seção intitulada “Polimorfismo em Haskell”](types-and-functions.html#funcstypes.polymorphism "Polymorphism in Haskell").

\[[5](#id579534)\] O ambiente em que opera **ghci** é chamado de mónade IO. No [Capítulo 7, _I/O_](io.html "Chapter 7. I/O"), vamos cobrir o mónade IO em profundidade, e aparentemente arbitrárias restrições que **ghci** coloca a nós fará mais sentido.

\[[6](#id580576)\] A expressão “não-estrita” e “preguiçosa” têm significados técnicas ligeiramente diferentes , mas não vamos entrar em detalhes da distinção aqui.

![](support/figs/rss.png) Quer ficar atualizado? Assine o feed comentário para [este capítulo](/feeds/comments/), ou o [livro inteiro](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart e John Goerzen. Esta obra está licenciada sob uma [Creative Commons Attribution-Noncommercial 3.0 License](http://creativecommons.org/licenses/by-nc/3.0/). Ícones por [Paul Davey](mailto:mattahan@gmail.com) aka [Mattahan](http://mattahan.deviantart.com/).

[Anterior](getting-started.html) 

 

 [Next](defining-types-streamlining-functions.html)

Capítulo 1. Introdução

[Casa](index.html)

 Capítulo 3. Definir os tipos, racionalizando as funções

_uacct = "UA-1805907-3"; urchinTracker();
