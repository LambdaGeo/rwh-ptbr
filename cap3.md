---
# You don't need to edit this file, it's empty on purpose.
# Edit theme's home layout instead if you wanna make some changes
# See: https://jekyllrb.com/docs/themes/#overriding-theme-defaults

---
Uma tradução não oficial do livro Real World Haskell 
de Bryan O'Sullivan, Don Stewart, and John Goerzen

-------------------------------------------------------


## Capítulo 3. Definindo os tipos e entendendo as funções


### Definir um tipo de dado novo


Embora as listas e tuplas são úteis, muitas vezes vamos querer construir nosso próprios tipos de dadis. Isto permite-nos estruturar os valores em nossos programas. Em vez de usar uma tupla anônimo, podemos dar a uma coleção de valores relacionados um nome e um tipo distinto. Definindo nossopróprios tipos também melhoramos a segurança do nosso código: Haskell não permitirá misturar acidentalmente valores de dois tipos que são estruturalmente semelhantes, mas têm nomes diferentes.

Depois motivação, vamos considerar alguns tipos de dados que uma pequena livraria on-line poderia necessitar para o seu gerenciamento. Não faremos qualquer tentativa de uma completa ou realista definições dedados, mas pelo menos estaremos ligando-os ao mundo real.

Nós definimos um novo tipo de dado usando o palavra-chave `data`.

```haskell
    -- arquivo: ca03/Livraria.hs
    data BookInfo = Book Int String [String]
                deriving (Show)
```

O InfoLivro após a palavra-chave `data` é o nome do nosso novo tipo. Chamamos InfoLivro um _construtor de tipo_. Assim que tiver definido um tipo, usaremos o construtor de tipo para se referir a ela. Como já mencionado, um nome de tipo e, portanto, um construtor de tipo, deve começar com uma letra maiúscula.

O `Livro` que se segue é o nome do _construtor de valor_ (às vezes chamado um construtor de dados). Nós usamos isso para criar um valor do tipo InfoLivro. Um nome de um construtor de valor também deve iniciar com uma letra maiúscula.

Depois `Livro`, os Int, String, e \[String\] que se seguem são os _componentes_ do tipo. Um componente tem a mesma finalidade em Haskell como um campo de uma estrutura ou classe em outra linguagem: é um “espaço” onde mantemos um valor. (Nós geralmente nos referimos aos componentes como campos.)

Neste exemplo, o Int representa um identificador (por exemplo, usado no banco de dados de estoque) de um livro, String seu título, e \[String\] os nomes de seus autores.

Para fazer o link para um conceito já vimos, o tipo InfoLivro contém os mesmos componentes como a 3-tupla do tipo (Int, String, \[String\]), mas tem um tipo distinto. Não podemos acidentalmente (ou deliberadamente) usar um em um contexto onde um outro tipo é o esperado. Por exemplo, uma livraria também é passível de ter revistas.

```haskell
-- arquivo: ca03/Livraria.hs
data MagazineInfo = Magazine Int String [String] 
			deriving (Show)
```

Mesmo que este tipo InfoRevista tem a mesma estrutura que o nosso tipo InfoLivro, Haskell trata os tipos come distintos porque a sua natureza e construtores de valor têm nomes diferentes.


>![[Note]]({{site.url}}/rwh-ptbr/assets/note.png)**Derivando o quê?**

>Nós vamos explicar o significado completo de `deriving Show` depois, em [seção denominada “Show”](using-typeclasses.html#typeclasses.wellknown.show "Show"). Por enquanto, é o suficiente saber que precisamos disso em uma declaração deste tipo para que o **ghci** automaticamente saiba como imprimir um valor deste tipo.

Podemos criar um novo valor do tipo InfoLivro tratando `Livro` como uma função, e aplicá-lo com argumentos do tipo Int, String, e \[String\].

```haskell
    -- arquivo: ca03/Livraria.hs
    myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]
```

Uma vez que tenhamos definido um modelo, podemos experimentar-lo com o **ghci**. Nós começamos usando o comando **:load** ou **:l** para carregar nosso arquivo de origem.

	ghci>:load Livraria
	[1 of 1] Compiling Main             ( Livraria.hs, interpreted )
	Ok, modules loaded: Main.

Lembre-se que a variável `meuInfo`  é definida no nosso arquivo fonte. Aqui está ela.

	ghci> myInfo
	Book 9780135072455 "Algebra of Programming" ["Richard Bird","Oege de Moor"]
	ghci> :type myInfo
	myInfo :: BookInfo

Podemos construir novos valores interativamente no **ghci** também.

	ghci> Book 0 "The Book of Imaginary Beings" ["Jorge Luis Borges"]
	Book 0 "The Book of Imaginary Beings" ["Jorge Luis Borges"]

O comando **ghci** **:type** ou **:t** nos permite ver o tipo de expressão é.

	ghci> :type Book 1 "Cosmicomics" ["Italo Calvino"]
	Book 1 "Cosmicomics" ["Italo Calvino"] :: BookInfo

Lembre-se que, se quisermos definir uma nova variável dentro do **ghci**, a sintaxe é ligeiramente diferente daquela de um arquivo fonte Haskell: é preciso colocar um `let` na frente da variável.

	ghci> let cities = Book 173 "Use of Weapons" ["Iain M. Banks"]

Para saber mais sobre um tipo, podemos usar alguns **ghci** capabilidades de browsing. O comando **:info** ou **:i** recebe informações do **ghci** para nos dizer tudo o que se sabe sobre um determinado nome.

    	ghci> :info BookInfo
	data BookInfo = Book Int String [String]
		-- Defined at BookStore.hs:4:5-12
	instance Show BookInfo -- Defined at Livraria.hs:4:5-12

Podemos também descobrir como usamos `Book` para construir um novo valor do tipo InfoLivro.

	ghci> :type Book
	Book :: Int -> String -> [String] -> BookInfo

Podemos tratar um construtor de valor como uma outra função qualquer, o que ele faz é criar e retornar um novo valor do tipo que desejamos.

### Nomenclatura de tipos e valores

Quando introduzimos o tipo de InfoLivro, nos deliberadamente escolhemos dar ao construtor do tipo BookInfo um nome diferente a partir do construtor valor `Book`, apenas para torná-lo evidente quem era quem.

No entanto, em Haskell, os nomes dos tipos e valores são independentes uns dos outros. Nós só usamos um construtor de tipo (ou seja, nome do tipo) em uma declaração do tipo ou em umaassinatura do tipo. Nós só usamos um construtor de valor em código real. Porque estes usos são distintos, não há ambigüidade se dermos a um construtor de tipo econstrutor valor o mesmo nome. Se estamos escrevendo uma assinatura de tipo, então estamos referindo a um construtor de tipo. Se estamos escrevendo uma expressão, então estamos usando o construtor de valor.

```haskell
    -- arquivo: ca03/Livraria.hs
    -- Vamos apresentar o tipo de CustomerID em breve.
    data BookReview = BookReview BookInfo CustomerID String
```

Esta definição diz que o tipo chamado RevisãoLivro tem um construtor de valor que também é chamado `RevisãoLivro`.

Não só é _legal_ para um construtor de valor para ter o mesmo nome de seu construtor de tipo, como é _normal_: você vai ver isso o tempo todo em código Haskell.

### Sinónimos

Podemos introduzir um _sinônimo_ para um tipo existente em qualquer momento, para dar um tipo de nome mais descritivo. Por exemplo, o tipo String no nosso tipo RevisãoLivro não nos diz para que a string é usada, mas podemos esclarecer isso.

```haskell
    	-- arquivo: ca03/Livraria.hs
    	type CustomerID = Int
	type ReviewBody = String
	data BetterReview = BetterReview BookInfo CustomerID ReviewBody
```

O palavra-chave `type` apresenta um sinônimo tipo. O novo nome é do lado esquerdo da `=`, com o nome existente do lado direito. Os dois nomes identificam o mesmo tipo, então sinônimos  são usados _apenas_ para tornar o código mais legível.

Nós também podemos usar um sinônimo de tipo para criar um nome mais curto para um modelo detalhado.
```haskell
    -- arquivo: ca03/Livraria.hs
    type BookRecord = (BookInfo, BookReview)
```

Isto indica que podemos utilizar NotaLivro como um sinônimo para a tupla (nfoLivro, RevisãoLivro). Um sinônimo de tipo só cria um novo nome que se refere a um tipo existente\[[7](#ftn.id582956)\]. Continuamos a usar os mesmos construtores de valor para criar um valor do tipo.

### Tipos de dado algébricos


O nosso conhecido Bool é o mais comum e simples exemplo de uma categoria de tipo chamada _tipo de dado algébricos_. Um tipo de dado algébrico pode ter mais de um construtor de valor.

```haskell
    	-- arquivo: ca03/Bool.hs
	data Bool = False | True
```

O tipo Bool tem dois construtores, os valores `True` e `False`. Cada construtor de valor é separado na definição por um caracter `|`, que pode ler-se “ou”: nós podemos construir um Bool que tem o valor `True`, ou o valor `False`. Quando um tipo tem mais de um construtor de valor, são normalmente referido como _alternativas_ ou _casos_. Podemos usar qualquer uma das alternativas para criar um valor desse tipo.


>![[Note]]({{site.url}}/rwh-ptbr/assets/note.png)**Uma nota sobre nomeação**

>Embora a expressão “tipo de dado algébrico” seja longa, nós estamos tendo o cuidado de evitar o uso da sigla “TAD”. Essa sigla já é amplamente entendida como suporte para o “tipo _abstrato_ de dado”. Desde que Haskell suporte ambos, tipos de dados algébricos e tipos de dados abstratos, vamos ser explícito e evitar a sigla inteiramente.

Cada um dos construtores de valor de um tipo de dados algébricos podem ter zero ou mais argumentos. Por exemplo, aqui está uma forma que poderia representar informações de faturas.

```haskell
-- arquivo: ca03/Livraria.hs
-- file: ch03/BookStore.hs
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
```

Aqui, nós estamos dizendo que suportamos três formas de faturar as compras dos nossos clientes. Se quiser pagar com cartão de crédito, deve fornecer o número do cartão, o nome do titular, e endereço do titular da fatura como argumentos para o construtor do valor `CartãoCrédito`. Alternativamente, é possível pagar a pessoa que entregou a sua encomenda. Uma vez que não precisamos guardar qualquer informação adicional neste caso, nós não especificamos nenhum argumento para o construtor `PagamentoContraEntrega`. Por último, poderemos enviar uma fatura para o cliente especificado,neste caso precisamos do seu IDCliente como argumento para o construtor `Fatura`.

Quando usamos um construtor de valor para criar um valor do tipo InfoFaturamento, devemos fornecer os argumentos que ele necessita.

	ghci> :type CreditCard
	CreditCard :: CardNumber -> CardHolder -> Address -> BillingInfo
	ghci> CreditCard "2901650221064486" "Thomas Gradgrind" ["Dickens", "England"]
	CreditCard "2901650221064486" "Thomas Gradgrind" ["Dickens","England"]
	ghci> :type it
	it :: BillingInfo
	ghci> Invoice

	<interactive>:1:0:
	    No instance for (Show (CustomerID -> BillingInfo))
	      arising from a use of `print' at <interactive>:1:0-6
	    Possible fix:
	      add an instance declaration for (Show (CustomerID -> BillingInfo))
	    In the expression: print it
	    In a 'do' expression: print it
	ghci> :type it
	it :: BillingInfo

A mensagem de erro `No instance` surgiu porque nós não fornecemos um argumento para o construtor `Fatura`. Como resultado, nós estávamos tentando imprimir o construtor `Fatura` de si mesmo. Construtor que requer um argumento e retorna um valor, por isso é uma função. Nós não podemos imprimir funções em Haskell, que é basicamente a razão pela qual o intérprete reclamou.

#### Tuplas, tipos de dado algébricos, e quando utilizar cada

Há alguma sobreposição entre tuplas e definidas pelo usuário tipos de dado algébricos. Se quiséssemos, poderíamos representar o nosso tipo de InfoLivro anteriormente como um tupla (Int, String, \[String\]).

	ghci> Book 2 "The Wealth of Networks" ["Yochai Benkler"]
	Book 2 "The Wealth of Networks" ["Yochai Benkler"]
	ghci> (2, "The Wealth of Networks", ["Yochai Benkler"])
	(2,"The Wealth of Networks",["Yochai Benkler"])

Tipos de dado algébrica nos permite distinguir entre idênticas partes umw informação. Duas tuplas com elementos do mesmo tipo são estruturalmente idênticos, então elas tem o mesmo tipo.
```haskell
    -- arquivo: ca03/Distincao.hs
    a = ("Porpoise", "Grey")
    b = ("Table", "Oak")
```

Uma vez que eles têm nomes diferentes, dois tipos de dados algébrica tem tipos distintos, mesmo que sejam estruturalmente equivalentes.

```haskell
-- arquivo: ca03/Distincao.hs
data Cetacean = Cetacean String String
data Furniture = Furniture String String

c = Cetacean "Porpoise" "Grey"
d = Furniture "Table" "Oak"
```

Isso nos permite trazer ao sistema de tipos a possibilidade de escrever programas com menos erros. Com as tuplas definidos acima, poderíamos passar uma descrição de uma baleia para uma função que esperava uma cadeira, e o sistema de tipos não poderia nos ajudar. Com os tipos de dados algébricos, não há essa possibilidade de confusão.

Aqui está um exemplo mais sutil. Considere as seguintes representações de um vetor de duas dimensões.

```haskell
    -- arquivo: ca03/VetorAlgébrico.hs
    -- Coordenadas x e y ou comprimentos:
    data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

     -- Angle and distance (magnitude).
     data Polar2D = Polar2D Double Double
		       deriving (Eq, Show)
```

As formas polares e cartesianas usam os mesmos tipos para seus dois elementos. No entanto, os _significados_ dos elementos são diferentes. Porque Cartesiano2D e Polar2D são tipos distintos, o sistema de tipos não vai nos deixar acidentalmente usar um valor Cartesian2D onde um Polar2D é esperado, ou vice-versa.

	ghci> Cartesian2D (sqrt 2) (sqrt 2) == Polar2D (pi / 4) 2

	<interactive>:1:33:
	    Couldn't match expected type `Cartesian2D'
		   against inferred type `Polar2D'
	    In the second argument of `(==)', namely `Polar2D (pi / 4) 2'
	    In the expression:
		  Cartesian2D (sqrt 2) (sqrt 2) == Polar2D (pi / 4) 2
	    In the definition of `it':
		it = Cartesian2D (sqrt 2) (sqrt 2) == Polar2D (pi / 4) 

O operador `(==)` exige que os seus argumentos sejam do mesmo tipo.


>![[Tip]]({{site.url}}/rwh-ptbr/assets/tip.png)**Comparando-se a igualdade**

>Observe que no cláusule `deriving` de vector tipos nossa, nós adicionamos uma outra palavra, `Eq`. Isso faz com que a aplicação Haskell para gerar o código que nos permite comparar os valores de igualdade.

Se usássemos tuplas para representar esses valores, podemos rapidamente nos terrenos em água quente, misturando as duas representações de forma inadequada.

	ghci> (1, 2) == (1, 2)
	True 

O sistema de tipos não pode salvar-nos aqui: na medida em que se preocupa, estamos comparando dois (Double, Double) pares, que é uma coisa perfeitamente válido para fazer. Na verdade, não podemos dizer que pela inspeção desses valores é suposto ser polar ou cartesiana, mas `(1,2)` tem um significado diferente em cada representação.

Não há nenhuma régua dura e rápida para decidir quando é melhor usar uma tupla ou um tipo de dados distintas, mas aqui é uma regra a seguir. Se você estiver usando valores compostos amplamente em seu código (como quase todos os programas não-trivial fazer), acrescentando data das declarações vai beneficiar você em ambos os tipo de segurança e legibilidade. Para os mais pequenos, usa localizada, uma tupla é geralmente fina.

#### Análogos aos tipos de dado algébricos em outras línguas

Tipos de dado algébricos fornecer uma poderosa forma única para descrever tipos de dados. Outras línguas muitas vezes precisam de várias características diferentes para atingir o mesmo grau de expressividade. Aqui estão alguns análogos de C e C++, o que pode tornar mais claro o que podemos fazer com os tipos de dados algébricos, e como eles se relacionam com os conceitos que poderia ser mais familiar.

##### O "struct" do C

Com apenas um construtor, um tipo de dados algébrico é semelhante a uma tupla: ele agrupa os valores relacionados juntos em um valor composto. Corresponde a uma `struct` em C ou C++, e seus componentes correspondem aos campos de uma `struct`. e seus componentes correspondem aos campos do tipo InfoLivro que definimos anteriormente.

```c
struct info_livro {  
    int id;  
    char *nome;  
    char **autores;  
};
```

A diferença principal entre os dois é que os campos do tipo Haskell são anônimos e de posicionamento.

```haskell
    -- arquivo: ca03/Livraria.hs
data BookInfo = Book Int String [String]
                deriving (Show)
```

Por _posicional_, queremos dizer que o número de seção é o primeiro campo do tipo Haskell, eo título é na segunda. Nós nos referimos a eles pelo local, e não pelo nome.

Na [seção chamada “casamento de padrões”](defining-types-streamlining-functions.html#deftypes.pattern "Pattern matching"), veremos como acessar os campos de um valor Livraria. Na [seção intitulada “Sintaxe record”](defining-types-streamlining-functions.html#deftypes.record "Record syntax"), vamos introduzir uma sintaxe alternativa para a definição de tipos de dados que parece um pouco mais similar a C.

##### Tipos enumerados

Tipos de dado algébricos também servem onde usaria um `enum` em C ou C + +, para representar um conjunto de valores simbólicos. Esses tipos de dados algébricos são muitas vezes referidos como tipos de enumeração. Aqui está um exemplo de C.

```c
enum vlavaiv {  
    vermelho,  
    laranja,  
    amarelo,  
    verde,  
    azul,  
    indigo,  
    violeta,  
};
```

E aqui está um equivalente Haskell.

```haskell
    -- arquivo: ca03/Roygbiv.hs
    data Roygbiv = Red
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
               deriving (Eq, Show)
```
	       
Podemos tentar estes no **ghci**.

	ghci> :type Yellow
	Yellow :: Roygbiv
	ghci> :type Red
	Red :: Roygbiv
	ghci> Red == Yellow
	False
	ghci> Green == Green
	True 

Em C, os elementos de um `enum` são inteiros. Podemos usar um número inteiro em um contexto onde um `enum` é esperado, e vice-versa: de um compilador C automaticamente converter valores entre os dois tipos. Isto pode ser uma fonte de bugs. Em Haskell, esse tipo de problema não ocorre. Por exemplo, nós não podemos usar um valor Vlavaiv onde um `Int` é esperado.

	 ghci> take 3 "foobar"
	"foo"
	ghci> take Red "foobar"

	<interactive>:1:5:
	    Couldn't match expected type `Int' against inferred type `Roygbiv'
	    In the first argument of `take', namely `Red'
	    In the expression: take Red "foobar"
	    In the definition of `it': it = take Red "foobar" 

##### A união disjunta

Se um tipo de dados algébrico tem várias alternativas, podemos pensar nele como semelhante a uma `union` em C ou C + +. A grande diferença entre os dois é que um union não nos diz qual a alternativa que está realmente presente, temos de forma explícita controlar manualmente qual a alternativa que estamos usando, normalmente em outro campo de uma estrutura envolvente. Isto significa que os sindicatos podem ser fontes de bugs, onde a nossa noção de qual a alternativa que nós devemos usar está incorrecto.

```c
enum tipo_forma {  
    forma_circulo,  
    forma_poligono,  
};  
  
struct circulo {  
    struct vetor centro;  
    float raio;  
};

  
struct poligono {  
    size_t num_vertices;  
    struct vetor *vertices;  
};  
  
struct forma   
{  
    enum tipo_forma tipo;  
    union {  
	struct circulo circulo;  
	struct poligono poligono;  
    } forma;  
};
```

No exemplo acima, a `union` pode conter dados válidos para qualquer um `struct circulo` ou um `struct poligono`. Nós temos que usar o `enum shape_type` com a mão para indicar que tipo de valor está armazenado na `union`.

A versão em Haskell deste código é tanto drasticamente mais curta e mais segura do que o equivalente C.

```haskell
    -- arquivo: ca03/UnionForma.hs
    type Vector = (Double, Double)

     data Shape = Circle Vector Double
           | Poly [Vector]
```

Se criarmos um valor Forma usando o construtor `Circulo`, o fato de que nós criamos um `Circulo` é armazenado. Quando mais tarde usar um `Circulo`, não podemos tratá-la acidentalmente, como um `Quadrado`. Vamos ver por que [o “Casamento de padrões”](defining-types-streamlining-functions.html#deftypes.pattern "Pattern matching").


>![[Tip]]({{site.url}}/rwh-ptbr/assets/tip.png)**Algumas notas**

>Da leitura dos capítulos anteriores, que agora deve ficar claro que _todos_ os tipos de dados que define a palavra-chave `data` são os tipos de dados algébricos. Alguns podem ter apenas uma alternativa, enquanto outros têm vários, mas eles estão todos usando as mesmas máquinas.

### Casamento de padrões


Agora que vimos como a construção de valores com os tipos de dados algébricos, vamos discutir como podemos trabalhar com esses valores. Se tivermos um valor de algum tipo, há duas coisas que gostaria de ser capaz de fazer.

*   Se o tipo tem mais de um construtor de valor, é preciso ser capaz de dizer qual construtor de valor foi usado para criar o valor.
    
*   Se o construtor de valor tem componentes de dados, precisamos ser capazes de extrair esses valores.
    

Haskell tem um recurso simples, mas extremament útil, que é o _casamento de padrões_, que nos permite fazer essas duas coisas.

Um padrão nos permite olhar para dentro de um valor e variáveis de vinculação com os dados que ele contém. Aqui está um exemplo de combinação de padrões em ação em um valor Bool: vamos reproduzir a função `not`.

```haskell
-- arquivo: ca03/soma.hs
myNot True  = False
myNot False = True
```

Pode parecer que temos duas funções chamado `meuNot` aqui, mas Haskell nos permite definir uma função como uma _série de equações_: estes dois são cláusulas que definem o comportamento da mesma função para diferentes padrões de entrada. Em cada linha, os padrões são os itens a seguir ao nome da função, até o símbolo `=`.

Para entender como funciona o casamento de padrões, vamos percorrer um exemplo, digamos `meuNot False`.

Quando aplicamos `meuNot`, o runtime Haskell verifica o valor que fornecemos contra o construtor de valor no primeiro padrão. Isto não corresponde, por isso tenta contra o segundo padrão. Essa correspondência é bem sucedida, por isso usa o lado direito da equação, como resultado da aplicação da função.

Aqui está um exemplo um pouco mais prolongado. Essa função adiciona os elementos de uma lista.

```haskell
-- arquivo: ca03/soma.hs
sumList (x:xs) = x + sumList xs
sumList []     = 0    
```

Vamos passo a passo através da avaliação de `somaLista [1,2]`. A notação de lista `[1,2]` é uma abreviação para a expressão `(1:(2:[]))`. Começamos por tentar compatibilizar o padrão da primeira equação da definição de `somaLista`. No padrão `(x:xs)`, o “`:`” é o construtor da lista familiar, `(:)`. Estamos agora a usá-lo para casar contra um valor, não para construir um . O valor `(1:(2:[]))` foi construído com `(:)`,então o construtor do valor corresponde ao construtor no padrão. Dizemos que o padrão _casou_, ou que o casamento foi _bem-sucedido_.

As variáveis `x` e `xs` agora são vinculada pelo construtor de argumentos, então a `x` é atribuído o valor `1`, e a `xs` o valor `2:[]`.

A expressão que estamos avaliando agora é `1 + somaLista (2:[])`. Devemos agora aplicar recursivamente `somaLista` para o valor `2:[]`. Mais uma vez, este foi construído utilizando `(:)`, assim o casamento foi bem-sucedido. Em nossa aplicação recursiva de `somaLista`, `x` é agora ligado a `2`, e `xs` para `[]`.

Estamos avaliando agora `1 + (2 + somaLista [])`. Nesta aplicação recursiva de `somaLista`, o valor que estamos buscando para casar `[]`. O valor do construtor não coincide com o construtor do primeiro padrão, então vamos pular essa equação. Em vez disso “caimos” para o padrão seguinte, que corresponde. O lado direito desta equação é, portanto, escolhido como o resultado dessa aplicação.

O resultado da `somaLista [1,2]` é, portanto `1 + (2 + (0))`, ou `3`.

>![[Note]]({{site.url}}/rwh-ptbr/assets/note.png)**A ordenação é importante**

>Como já mencionado, a implementação Haskell checa o padrão para casar na  mesma ordem em que especificá-mos em nossas funções. Procedimento de "casamento" é de cima para baixo, e termina no primeiro sucesso.

Como nota final, já existe uma função padrão chamada `sum`, que realiza esta soma de lista para nós. Nossa `somaLista` é meramente ilustrativa.

#### Construção e deconstrução

Damos um passo a trás e daremos uma olhada na relação entre a construção de um valor e o seu casamento de padrão.

Nós aplicamos um construtor de valor para construir um valor. A expressão `Livro 9 "Close Calls" ["John Long"]` aplica-se o construtor `Livro` para os valores `9`, `"Close Calls"` e `["John Long"]` para produzir um novo valor do tipo InfoLivro.

Quando casamos o padrão sobre o construtor `Livro`, nós _invertemos_ o processo de construção. Primeiro de tudo, verificamos se o valor foi criado usando aquele construtor. Se for, nós inspecioná-lo para obter os valores individuais que foi originalmente fornecido para o construtor, quando criamos o valor.

Vamos considerar o que acontece se o casamento de padrão `(Livro id nome autores)` sobre a nossa expressão do exemplo.

*   O casamento terá êxito, porque o construtor no valor corresponde a um do nosso padrão.
    
*   A variável `id` será vinculada a `9`.
    
*   A variável `nome` será vinculada a `"Close Calls"`.
    
*   A variável `autores` será vinculada a `["John Long"]`.
    

Como o padrão combina age como o inverso da construção, é por vezes referido como _de_construção.

>![[Note]]({{site.url}}/rwh-ptbr/assets/note.png)**A desconstrução não destrói nada**

>Se você está mergulhada em programação orientada a objeto jargão, não confunda a desconstrução com destruição! Correspondência de um padrão não tem nenhum efeito sobre o valor que estamos examinando: só nos permite “olhar para dentro” dele.

#### Outras aventuras

A sintaxe de casamento de padrões em uma tupla é similar à sintaxe para a construção de uma tupla. Aqui está uma função que retorna o último elemento de uma 3-tuplo.

```
-- arquivo: ca03/Tupla.hs
third (a, b, c) = c
```

Não há limite para o tão “profundo” dentro de um valor padrão podemos olhar. Esta definição olha tanto dentro de uma tupla quanto dentro de uma lista em uma tupla.

    -- arquivo: ca03/Tupla.hs
    complicated (True, a, x:xs, 5) = (a, xs)

Podemos tentar fazer isso de forma interativa.

	ghci> :load Tuple.hs
	[1 of 1] Compiling Main             ( Tuple.hs, interpreted )
	Ok, modules loaded: Main.
	ghci> complicated (True, 1, [1,2,3], 5)
	(1,[2,3])

Sempre que um valor literal está presente em um padrão (`True` e `5` do padrão da tupla acima), esse valor tem de corresponder exatamente ao padrão para ter sucesso. Se o padrão dentro de uma série de equações não corresponde ao valor de entrada, temos então um erro de execução.

	ghci> complicated (False, 1, [1,2,3], 5)
	*** Exception: Tuple.hs:10:0-39: Non-exhaustive patterns in function complicated


Para uma explicação sobre essa mensagem de erro, avance um pouco, para [a seção chamada “Padrões exaustivos e curingas”](defining-types-streamlining-functions.html#deftypes.patterns.nonexhaustive "Exhaustive patterns and wild cards").

Nós podemos casar padrões em um tipo de dados algébrico usando o valor seus construtores. Lembrando-se do tipo InfoLivro definido anteriormente: podemos extrair os valores de um InfoLivro como se segue.

```haskell
-- arquivo: ca03/Livraria.hs
-- file: ch03/BookStore.hs
bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors
```

Vamos vê-lo em ação.

	ghci> bookID (Book 3 "Probability Theory" ["E.T.H. Jaynes"])
	3
	ghci> bookTitle (Book 3 "Probability Theory" ["E.T.H. Jaynes"])
	"Probability Theory"
	ghci> bookAuthors (Book 3 "Probability Theory" ["E.T.H. Jaynes"])
	["E.T.H. Jaynes"]

O compilador pode inferir os tipos de funções de acesso baseado no construtor que estamos usando no nosso padrão.

	ghci> :type bookID
	bookID :: BookInfo -> Int
	ghci> :type bookTitle
	bookTitle :: BookInfo -> String
	ghci> :type bookAuthors
	bookAuthors :: BookInfo -> [String]

Se usarmos um valor literal em um padrão, a parte correspondente ao valor que estamos combinando deverá conter um valor idêntico. Por exemplo, o padrão `(3:xs)` antes de mais nada é verificado se um valor é uma lista não vazia, casando com o construtor `(:)`. Ele também garante que a cabeça da lista tem o valor exato `3`. Se ambas as condições obtivere exitos, a cauda da lista encontra-se vinculada à variável `xs`.

#### Nomenclatura de variáveis em padrões

A media que você lê funções que fazem casamento de lista, você vai encontrar muitas vezes que os nomes das variáveis dentro de um padrão é  semelhante a `(x:xs)` ou `(d:ds)`. Trata-se de uma convenção de nomenclatura popular. A idéia é que o nome do `xs` tem um “`s`” no final de seu nome como se fosse o “plural” de `x`, porque `x` contém a cabeça da lista, e `xs` os elementos restantes.

#### O padrão curinga

Podemos indicar que não importa o que está presente em parte de um padrão. A notação para isto é o caractere sublinhado “`_`”, que chamamos de um _curinga_ ou _wild card_. Vamos utilizá-lo como se segue.

```haskell
-- arquivo: ca03/Livraria.hs
nicerID      (Book id _     _      ) = id
nicerTitle   (Book _  title _      ) = title
nicerAuthors (Book _  _     authors) = authors
```

Aqui, temos versões limpas das funções de assessor apresentada anteriormente. Agora, não há dúvida sobre qual o elemento que estamos usando em cada função.

Em um padrão, o curinga funciona de maneira similar a uma variável, mas não é vinculada a uma nova variável. Como nos exemplos acima, podemos usar mais de um wild card em um único padrão.

Outra vantagem do padrão curinga é que um compilador Haskell pode nos avisar se nós introduzimos um nome de variável em um padrão, mas não usamos-as no corpo de uma função. Definindo uma variável, mas esquecendo-se de usá-lo, muitas vezes pode indicar a presença de um erro, por isso este é um recurso útil. Se usarmos um wild card, em vez de uma variável que não temos a intenção de usar, o compilador não vai reclamar.

#### Padrões exaustivos e curingas

Ao escrever uma série de padrões, é importante cobrir todos os construtores de um tipo. Por exemplo, se nós estamos inspecionando uma lista, devemos ter uma equação que coincide com o construtor não-vazio `(:)`, e que coincide com o construtor lista vazia `[]`.

Vamos ver o que acontece se não conseguimos cobrir todos os casos. Aqui, nós deliberadamente omitimos a  verificação para o construtor `[]`.

```haskell
-- file: ch03/BadPattern.hs
badExample (x:xs) = x + badExample xs
``` 

Se aplicarmos isso a um valor que não pode corresponder, vamos receber um erro em tempo de execução: o nosso software tem um bug!

	ghci> badExample []
	*** Exception: BadPattern.hs:4:0-36: Non-exhaustive patterns in function badExample


Neste exemplo, nenhuma equação na definição da função coincide com o valor `[]`.

>![[Tip]]({{site.url}}/rwh-ptbr/assets/tip.png)**Aviso sobre padrões incompletos**

>GHC oferece uma opção de compilação útil `-fwarn-incomplete-patterns`, que fará com que ele imprime um aviso durante a compilação se uma seqüência de padrões não correspondem a todos tipo de valor de um construtor.

Se nós precisamos de fornecer um comportamento padrão nos casos em que não se preocupam com os construtores específicos, podemos utilizar um padrão curinga.

```haskell
-- file: ch03/BadPattern.hs
goodExample (x:xs) = x + goodExample xs
goodExample _      = 0
```

A curinga acima irá coincidir com o construtor `[]`, para a aplicação dessa função não conduzir a um erro.

	ghci> goodExample []
	0
	ghci> goodExample [1,2]
	3

### Sintaxe de registro


Escrevendo funções de acesso para cada componentes de um tipo de dado pode ser repetitivo e enfadonho.

ghci> goodExample []
0
ghci> goodExample [1,2]
3

Chamamos esse tipo de código _boilerplate (clichê)_: necessário, mas volumoso e cansativo. Haskell programadores não gostam de clichês. Felizmente, a linguagem resolve este problema particular: Podemos definir um tipo de dados, e assessores para cada um dos seus componentes, ao mesmo tempo. (As posições das vírgulas é uma questão de preferência. Se você gostar, coloque-os no final de uma linha, em vez do começo.)

```haskell
-- file: ch03/BookStore.hs
data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)
```

Isso é quase exatamente idêntico em significado a seguinte forma mais familiar.

```haskell
-- file: ch03/AltCustomer.hs
data Customer = Customer Int String [String]
                deriving (Show)

customerID :: Customer -> Int
customerID (Customer id _ _) = id

customerName :: Customer -> String
customerName (Customer _ name _) = name

customerAddress :: Customer -> [String]
customerAddress (Customer _ _ address) = address
```

Para cada um dos campos que nós nomeamos, na nossa definição de tipo, Haskell cria uma função de assessor com esse nome.

	ghci> :type customerID
	customerID :: Customer -> CustomerID

Podemos ainda usar a aplicação da sintaxe usual para criar um valor deste tipo.

```haskell
-- file: ch03/BookStore.hs
customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]
```

A sintaxe de registor adiciona uma notação mais detalhada para a criação de um valor. Isto às vezes pode tornar o código mais legível.

```haskell
-- file: ch03/BookStore.hs
customer2 = Customer {
              customerID = 271828
            , customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"]
            , customerName = "Jane Q. Citizen"
            }
```

Se usarmos esta forma, podemos variar a ordem em que  nós listamos os campos. Aqui, temos movido o nome e os campos de endereço das suas posições na declaração do tipo.

Quando definimos um tipo utilizando a sintaxe registro, ele também muda a forma como o tipo de valores são impressos.

	ghci> customer1
	Customer {customerID = 271828, customerName = "J.R. Hacker", customerAddress = ["255 Syntax Ct","Milpitas, CA 95134","USA"]}

Para efeito de comparação, vamos olhar para um valor InfoLivro; que definimos sem a sintaxe de registro.

	ghci> cities
	Book 173 "Use of Weapons" ["Iain M. Banks"]

As funções de acesso que recebemos “de graça” quando usamos a sintaxe registro realmente são funções normais Haskell.

	 ghci> :type customerName
	customerName :: Customer -> String
	ghci> customerName customer1
	"J.R. Hacker"

A módulo padrão `System.Time` faz bom uso da sintaxe de registro. Aqui está um tipo definido nesse módulo:

```haskell
data CalendarTime = CalendarTime {
  ctYear                      :: Int,
  ctMonth                     :: Month,
  ctDay, ctHour, ctMin, ctSec :: Int,
  ctPicosec                   :: Integer,
  ctWDay                      :: Day,
  ctYDay                      :: Int,
  ctTZName                    :: String,
  ctTZ                        :: Int,
  ctIsDST                     :: Bool
} 
```    

Na ausência de registro de sintaxe, seria doloroso extrair campos específicos de um tipo como este. A notação torna mais fácil trabalhar com grandes estruturas.

### Tipos parametrizados


Nós temos repetidamente dito que o tipo de lista é polimórfico: os elementos de uma lista pode ser de qualquer tipo. Podemos acrescentar ao nosso polimorfismo tipos próprios. Para fazer isso, introduzimos variáveis de tipo em uma declaração de tipo. O Prelude define um tipo chamado Maybe: nós podemos usar-lo para representar um valor que poderia estar presente ou ausente, por exemplo, um campo em uma linha do banco de dados que podem ser nulos.

```haskell
-- file: ch03/Nullable.hs
data Maybe a = Just a
             | Nothing
```

Aqui, a variável `a` não é uma variável regular: é uma variável de tipo. Ela indica que o tipo Maybe tem outro tipo como seu parâmetro. Isso permite-nos usar Maybe em valores de qualquer tipo.

```haskell
-- file: ch03/Nullable.hs
someBool = Just True

someString = Just "something"
```

Como de costume, podemos experimentar este tipo com o **ghci**.

	ghci> Just 1.5
	Just 1.5
	ghci> Nothing
	Nothing
	ghci> :type Just "invisible bike"
	Just "invisible bike" :: Maybe [Char]

Maybe é do tipo polimórfico, ou genérico. Nós damos ao construtor de tipo Maybe um parâmetro para criar um tipo específico, como Maybe Int ou Maybe \[Bool\]. Como seria de esperar, estes tipos são distintos.

Podemos usar tipos parametrizados um dentro do outra, mas quando o fizermos, pode ser necessário usar parênteses para dizer ao compilador Haskell como analisar a nossa expressão.

```haskell
-- file: ch03/Nullable.hs
wrapped = Just (Just "wrapped")
```

Para prorrogar mais uma vez uma analogia com linguagens mais comuns, tipos parametrizados tem algumas semelhanças com templates em C++, e generics em Java. Esteja ciente de que esta é uma analogia superficial. Templates e generics foram adicionados às suas respectivas linguagems tempo depois que as linguagems foram inicialmente definidas, e tem uma aspecto desajeitado. Tipos parametrizada do Haskell são mais simples e fácil de usar, como a linguagem foi projetada com eles desde o início.

### Tipos recursivos
-

O tipo familiar lista é _recursivo_: ele é definido em termos de si mesmo. Para entender isso, vamos criar o nosso próprio tipo lista. Usaremos `Cons` no lugar do construtor `(:)` e `Nulo` no lugar de `[]`.

```haskell
-- file: ch03/ListADT.hs
data List a = Cons a (List a)
            | Nil
              deriving (Show)
```

Como Lista a aparece à esquerda e à direita do simbolo `=`, a definição do tipo se refere a si mesma. Se quisermos usar o construtor `Cons` para criar um novo valor,  devemos fornecer um valor do tipo `a`, e outro do tipo `Lista a`. Vamos ver onde isso nos leva, na prática.

O valor mais simples de um tipo de Lista a que podemos criar é `Nulo`. Salve a definição do tipo em um arquivo, e em seguida, carregue-o no **ghci**.

	ghci> Nil
	Nil

Porque `Nil` tem um tipo Lista a , podemos utilizá-lo como um parâmetro para `Cons`.

	ghci> Cons 0 Nil
	Cons 0 Nil

E porque `Cons 0 Nulo` tem o tipo Lista a, nós podemos usar isso como um parâmetro para `Cons`.

	ghci> Cons 1 it
	Cons 1 (Cons 0 Nil)
	ghci> Cons 2 it
	Cons 2 (Cons 1 (Cons 0 Nil))
	ghci> Cons 3 it
	Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil)))

Poderíamos continuar dessa maneira indefinidamente, criando cada vez mais cadeias `Cons`, cada uma com um único `Nulo` no final.

>![[Tip]]({{site.url}}/rwh-ptbr/assets/tip.png)**É Lista uma lista aceitável?**

>Nós podemos facilmente provar a nós mesmos que o nosso tipo List a tem a mesma forma que o tipo lista \[a\] existente no ghc. Para fazer isso, nós escrevemos uma função que recebe um valor do tipo \[a\], e produzimos um valor de um tipo Lista a.

```haskell
-- file: ch03/ListADT.hs
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil
```

>Por inspeção, é evidente que substituimos uma `Cons` para cada `(:)`, e um `Nil` para cada `[]`. Isto abrange ambas as built-in do tipo lista de construtores. Os dois tipos são _isomorfos_; pois eles têm a mesma forma.

	ghci> fromList "durian"
	Cons 'd' (Cons 'u' (Cons 'r' (Cons 'i' (Cons 'a' (Cons 'n' Nil)))))
	ghci> fromList [Just True, Nothing, Just False]
	Cons (Just True) (Cons Nothing (Cons (Just False) Nil))

Para um terceiro exemplo de que é um tipo recursivo, aqui está uma definição do tipo de árvore binária.

```haskell
-- file: ch03/Tree.hs
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
```

Uma árvore binária ou é um nó com dois filhos, que são tambem árvores binárias, ou um valor vazio.

Desta vez, vamos comparar com uma definição em uma linguagem mais familiar. Aqui está uma definição de classe similares em Java.

```cpp
class Árvore<A>  
{  
    A valor;  
    Árvore<A> esquerda;  
    Árvore<A> direita;  
  
    public Árvore(A v, Árvore<A> l, Árvore<A> r)  
    {  
	valor = v;  
	esquerda = l;  
	direita = r;  
    }  
}
```

A única diferença significativa é que o Java nos permite usar o valor especial `null` para indicar qualquer lugar “nada”, então podemos usar `null` para indicar que um nó está faltando um filho a direita ou à esquerda. Aqui está uma pequena função que constrói uma árvore com duas folhas (uma folha, por convenção, não tem filhos).

```cpp
class Exemplo  
{  
    static Árvore<String> arvoreSimples()  
    {  
	return new Árvore<String>(  
            "raiz",  
	    new Árvore<String>("filho esquerdo", null, null),  
	    new Árvore<String>("filho direito", null, null));  
    }  
}
```

Em Haskell, não temos um equivalente ao `null`. Nós poderíamos usar o tipo Maybe para fornecer um efeito similar, mas o que incharia o casamento de padrão. Ao invés disso, nós decidimos usar um construtor sem argumento `Vazio`. Onde o exemplo Java usamos `null` para o construtor de Arvore, nós usamos o `Vazio` em Haskell.

```haskell
    -- arquivo: ca03/Árvore.hs
```

### Exercícios

**1.** Escreva o inverso do `doList` para o tipo de Lista: uma função que recebe uma Lista a e gere um \[a\].

**2.** Definir um tipo de árvore que tem somente um construtor, como o nosso exemplo Java. Em vez do construtor `Vazio`, use o tipo Maybe para se referir a um nó de filhos.

### Reportando erros


Haskell oferece uma função padrão `error:: String-> a`, que podemos chamar quando algo saiu terrivelmente errado no nosso código. Damos-lhe um parâmetro string, que é a mensagem de erro para mostrar. Sua assinatura de tipo parece peculiar: como pode produzir um valor de qualquer tipo de `a` dado somente uma cadeia de caracteres ?

Ela tem um resultado do tipo `a` modo que podemos chamá-lo em qualquer lugar e sempre terá o tipo certo. No entanto, ele não retorna um valor como uma função normal: em vez disso, ele _aborta avaliação imediatamente_, e imprime a mensagem de erro que lhe damos.

A função `meuSegundo` retorna o segundo elemento da sua lista de entrada, mas ela falha caso a sua lista de entrada não é tiver o tamanho suficiente.

```haskell
-- file: ch03/MySecond.hs
mySecond :: [a] -> a

mySecond xs = if null (tail xs)
              then error "list too short"
              else head (tail xs)
```

Como de costume, nós podemos ver como isso funciona na prática **ghci**.

	ghci> mySecond "xi"
	'i'
	ghci> mySecond [2]
	*** Exception: list too short
	ghci> head (mySecond [[9]])
	*** Exception: list too short

Observe que o terceiro caso acima, onde nós tentamos usar o resultado da chamada para `meuSegundo` como argumento para outra função. Avaliação ainda termina e cai-nos de volta ao prompt **ghci**. Esta é a grande fraqueza do uso de `error`: não deixar o nosso interlocutor distinguir entre um erro recuperável e um problema tão grave que ele realmente deve encerrar o nosso programa.

Como nós já vimos, o padrão de casamento falha e provoca um similar erro irreparável.

	ghci> mySecond []
	*** Exception: Prelude.tail: empty list

### Uma abordagem mais controlada

Podemos usar o tipo Maybe para representar a possibilidade de um erro.

Se queremos indicar que uma operação falhou, nós podemos usar o construtor `Nothing`. Caso contrário, nós envolvemos o nosso valor com o construtor `Just`.

Vamos ver como a nossa função `meu``Segundo`  muda se nós retornamos um valor Maybe ao invés da chamada `error`.

```haskell
-- file: ch03/MySecond.hs
safeSecond :: [a] -> Maybe a

safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))
```

Se a lista que você passou é muito curta, voltamos `Nothing` a nosso interlocutor. Isso permite que eles decidam o que fazer, onde uma chamada para `error` forçaria uma queda.

	ghci> safeSecond []
	Nothing
	ghci> safeSecond [1]
	Nothing
	ghci> safeSecond [1,2]
	Just 2
	ghci> safeSecond [1,2,3]
	Just 2

Para retornar a um tópico anterior, podemos melhorar a legibilidade desta função com a casamento de padrões.

```haskell
-- file: ch03/MySecond.hs
tidySecond :: [a] -> Maybe a

tidySecond (_:x:_) = Just x
tidySecond _       = Nothing
```

O primeiro padrão corresponde somente se a lista é de pelo menos dois elementos de comprimento (que contém dois construtores lista), e vincula-se a variável `x` para o segundo elemento da lista. O segundo padrão é casado se o primeiro falhar.

### Introdução das variáveis locais


Dentro do corpo de uma função, podemos introduzir novas variáveis locais sempre que precisamos deles, usando uma expressão `let`. Aqui é uma simples função que determina se devemos emprestar algum dinheiro para um cliente. Encontramos uma reserva de dinheiro de pelo menos 100, voltamos nosso novo equilíbrio depois de subtrair o valor que têm emprestado.

```haskell
-- file: ch03/Lending.hs
lend amount balance = let reserve    = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance
```

As palavras-chave a olhar aqui é a `let`, que inicia um bloco de declarações de variáveis, e `in`, que termina-o. Cada linha introduz uma nova variável. O nome está do lado esquerdo do `=`, e a expressão que ele está vinculada está à direita.

![[Note]]({{site.url}}/rwh-ptbr/assets/note.png)

>**Notas especiais**

>Vamos re-enfatizar a nossa redação: um nome em bloco `let`  é vinculado a uma _expressão_, não a um _valor_. Porque Haskell é uma linguagem de avaliação preguiçosa, a expressão associada com um nome realmente não vai ser avaliado até que seja necessário. No exemplo acima, não vamos calcular o valor do `equilíbrioNovo` se não cumprir as nossas reservas.

>Quando definimos uma variável em um bloco `let`, nós referimos a ela como uma variável _vinculação `let`_ (_`let`-bound_ variable). Isto simplesmente significa que ele diz: temos vinculado a uma variável em um bloco `let`.

>Além disso, a utilização de espaço em branco aqui é importante. Falaremos em mais detalhes sobre as regras de layout [a seção chamada “A regra de impedimento e espaço em branco em uma expressão”](defining-types-streamlining-functions.html#deftypes.offside "The offside rule and white space in an expression").

Podemos usar o nome de uma variável em bloco `let` dentro do bloco de declarações e na expressão que segue a palavras-chave `in`.

Em geral, vamos nos referir aos lugares dentro de nosso código, onde podemos usar um nome como o nome do _escopo_. Se nós podemos usar um nome, ele está _no escopo_, caso contrário ele está _fora de escopo_. Se um nome é visível ao longo de um arquivo fonte, dizemos que ele está no _nível superior_.

### Ocultamento

Nós podemos “aninhar” múltiplos blocos `let` dentro do outro em uma expressão.

```haskell
-- file: ch03/NestedLets.hs
foo = let a = 1
      in let b = 2
         in a + b
```

É perfeitamente legal, mas não exatamente inteligente, para repetir um nome de variável em uma expressão `let` aninhada.

```haskell
-- file: ch03/NestedLets.hs
bar = let x = 1
      in ((let x = "foo" in x), x
```

Aqui, o `x` interior está escondido, ou _oculto_, do `x` exterior. Ele tem o mesmo nome, mas de um tipo diferente e valor.

	ghci> bar
	("foo",1)

Nós também podemos ocultar parâmetros de uma função, levando a resultados ainda desconhecido. Qual é o tipo desta função?

```haskell
-- file: ch03/NestedLets.hs
quux a = let a = "foo"
         in a ++ "eek!"
```

Porque o argumento da função `a` nunca é usado no corpo da função, devido a estar ocultada  pelo  `let-bound a`, o argumento pode ter qualquer tipo.

	ghci> :type quux
	quux :: t -> [Char]

>![[Tip]]({{site.url}}/rwh-ptbr/assets/tip.png)**Advertências do compilador são seus amigos**

>Ocultamento pode, obviamente, levar à confusão e bugs, assim GHC tem um opção útil `-fwarn-name-shadowing`. Quando ativado, GHC irá imprimir uma mensagem de aviso toda vez que ocultarmos um nome.

#### A cláusula where

Podemos usar um outro mecanismo para introduzir variáveis locais: a cláusula `where`. As definições de cláusula `where` aplicam ao código que a _antecede_. Aqui está uma função semelhante a `emprestado`, usando `where` em vez de `let`.

```haskell
-- file: ch03/Lending.hs
lend2 amount balance = if amount < reserve * 0.5
                       then Just newBalance
                       else Nothing
    where reserve    = 100
          newBalance = balance - amount
```

Enquanto uma cláusula `where` pode parecer estranho inicialmente, oferece uma maravilhosa ajuda a legibilidade. Deixa-nos dirigir o foco do nosso leitor os detalhes importantes de uma expressão, com as definições apresentadas a seguir. Depois de um tempo, você pode sentir falta por não achar cláusulas `where` em linguagens que não as possuem.

Tal como acontece com expressões `let`, espaço em branco é significativo nas cláusulas `where`. Nós falaremos mais sobre as regras de layout em breve, na [seção chamada “A regra offside e espaço em branco em uma expressão”](defining-types-streamlining-functions.html#deftypes.offside "The offside rule and white space in an expression").

#### Funções locais, variáveis globais

Você vai ter notado que a sintaxe Haskell para definir uma variável é muito parecido com sua sintaxe para a definição de uma função. Essa simetria é mantida em blocos `let` e `where`: podemos definir _funções_ locais tão facilmente quanto as _variáveis_ locais.

```haskell
-- file: ch03/LocalFunction.hs
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"
```

Nós definimos uma função local `plural`, que consiste de várias equações. Funções locais podem usar livremente as variáveis dos escopos onde foram colocá-das: aqui, nós usamos `palavra` da definição da função exterior `plurals`. Na definição de `plurals`, a função `map` (que iremos revisitar no próximo capítulo) aplica-se a função local `plural` para cada elemento da lista `contagens`.

Nós também podemos definir variáveis, bem como as funções, no nível superior de um arquivo fonte.

```haskell
-- file: ch03/GlobalVariable.hs
itemName = "Weighted Companion Cube"
```

### A regra offside e espaço em branco em uma expressão


Em nossas definições de `emprestado` e `emprestado2`, na margem esquerda do nosso texto vagava um pouco. Este não foi um acidente: em Haskell, um espaço em branco tem um significado.

Haskell usa indentação como uma sugestão para analisar seções de código. Este uso de layout para transmitir a estrutura é muitas vezes chamado de _regra offside_. No início de um arquivo fonte, o nível superior em primeiro lugar declaração ou definição pode começar em qualquer coluna, e o compilador Haskell ou intérpretador lembra que nível de recuo. Cada nível superior posterior declaração deve ter a mesma endentação.

Aqui está um exemplo do nível de recuo norma superior. Nosso primeiro arquivo `GoodIndent.hs`, é bem comportada.

```haskell
-- file: ch03/GoodIndent.hs
-- This is the leftmost column.

  -- It's fine for top-level declarations to start in any column...
  firstGoodIndentation = 1

  -- ...provided all subsequent declarations do, too!
  secondGoodIndentation = 2
```

Nosso segundo, `BadIndent.hs`, doesn't play by the rules.

```haskell
-- file: ch03/BadIndent.hs
-- This is the leftmost column.

    -- Our first declaration is in column 4.
    firstBadIndentation = 1

  -- Our second is left of the first, which is illegal!
  secondBadIndentation = 2
```

Veja o que acontece quando tentamos carregar os dois arquivos em **ghci**.

	ghci> :load GoodIndent.hs
	[1 of 1] Compiling Main             ( GoodIndent.hs, interpreted )
	Ok, modules loaded: Main.
	ghci> :load BadIndent.hs
	[1 of 1] Compiling Main             ( BadIndent.hs, interpreted )

	BadIndent.hs:8:2: parse error on input `secondBadIndentation'
	Failed, modules loaded: none.

O seguinte linha vazia é tratada como uma continuação do item atual, como é uma linha que se segue mais recuado para a direita.

As regras para expressões `let` e as cláusulas `where` são semelhantes. Depois de uma palavra-chave `let` ou `where`, o compilador Haskell ou intérprete recorda o recuo do próximo token que vê. Se a linha que se segue é vazio, ou o seu recuo é mais para a direita, considera-se para continuar a linha anterior. Se o recuo é a mesma do início do item anterior, este é tratado como início de um novo item no mesmo bloco.

```haskell
-- file: ch03/Indentation.hs
foo = let firstDefinition = blah blah
          -- a comment-only line is treated as empty
                              continuation blah

          -- we reduce the indentation, so this is a new definition
          secondDefinition = yada yada

                             continuation yada
      in whatever
```

Aqui estão usas aninhados de `let` e `where`.

```haskell
-- file: ch03/letwhere.hs
bar = let b = 2
          c = True
      in let a = b
         in (a, c)
```

O nome de `a` só é visível no a expressão `let` interior. Não é visível no `let` exterior. Se tentarmos usar o nome de `a` lá, nós vamos receber um erro de compilação. O recuo nos dá tanto o compilador e uma sugestão visual, como o que está no espaço.

```haskell
-- file: ch03/letwhere.hs
foo = x
    where x = y
              where y = 2
```

Do mesmo modo, no âmbito da primeira cláusula `where` é a definição de `foo`, mas o escopo do segundo é apenas o primeiro cláusula `where`.

O indentação que usamos para as cláusulas `let` e `where` faz nossas intenções fácil de descobrir.

### Uma nota sobre tabs versus espaços

Se você usar um editor de texto ciente Haskell (eg Emacs), ele provavelmente já está configurado para usar caracteres de espaço para todos os espaços em branco quando você editar arquivos fonte Haskell. Se o editor _não_ é Haskell-conhecimento, você deve configurá-lo para usar apenas caracteres de espaço.

A razão para isto é a portabilidade. Em um editor que utiliza uma fonte de largura fixa, tabulações, por convenção, são colocados em diferentes intervalos de sistemas Unix-like (a cada oito caracteres) que no Windows (a cada quatro caracteres). Isto significa que não importa o que suas crenças pessoais sobre onde guias pertencem, você não pode confiar em alguém do editor honrar suas preferências. Qualquer avanço que usa abas está indo olhar quebrado sob a configuração de _alguém_ De fato, isso pode levar a problemas de compilação, como o linguagem padrão Haskell exige implementações usar o Unix convenção tabulação. Usando caracteres de espaço evita este problema completamente.

### A regra offside não é obrigatória

Podemos usar explícita estruturação de layout em vez de indicar o que queremos dizer. Para fazer isso, nós começamos um bloco de equações com uma chave de abertura; separe cada item com um ponto e vírgula, e terminar o bloco com uma chave de fechamento. A seguir os dois usos `let` têm o mesmo significado.

```haskell
-- file: ch03/Braces.hs
bar = let a = 1
          b = 2
          c = 3
      in a + b + c


foo = let { a = 1;  b = 2;
        c = 3 }
      in a + b + c
```

Quando usamos a estruturação explícita, as regras de distribuição normal não se aplica, que é por isso que podemos ir longe com recuo invulgar na expressão `let` segunda.

Podemos usar a estruturação explícita em qualquer lugar que nós normalmente usamos layout. É válido para cláusulas `where`, e declarações de nível superior mesmo. Basta lembrar que, embora o mecanismo existe, explícita estruturação quase nunca é realmente _utilizada_ em programas Haskell.

### A expressão case


As definições de função não são o único lugar onde podemos usar a correspondência de padrão. O `case` nos permite construir padrões de jogo dentro de uma expressão. Aqui está o que parece. Esta função (definida para nós em `Data.Maybe`) desembrulha um valor Maybe, usando um padrão se o valor é `Nothing`.

```haskell
-- file: ch03/Guard.hs
fromMaybe defval wrapped =
    case wrapped of
      Nothing     -> defval
      Just value  -> value
```

A palavras-chave `case` é seguido por uma expressão arbitrária: a correspondência de padrões é feita contra o resultado desta expressão. O chave `of` significa o fim da expressão e do início do bloco de padrões e expressões.

Cada item no bloco consiste em um padrão, seguido por uma seta `->`, seguido de uma expressão para avaliar se o padrão de partidas. Estas expressões devem ter todos o mesmo tipo. O resultado da expressão `case` é o resultado da expressão associada com o primeiro padrão a casar. Casamentos são tentadas a partir de cima para baixo.

Para expressar “aqui é a expressão para avaliar se nenhum dos outros padrões match”, a gente só usa o cartão padrão curinga `_` como o último da nossa lista de padrões. Se um jogo padrão falhar, teremos o mesmo tipo de erro de execução, como vimos anteriormente.

### Erros comuns dos novatos com os padrões

Existem algumas maneiras em que Haskell novos programadores podem não entender ou padrões de utilização indevida. Aqui estão algumas tentativas de correspondência padrão deu errado. Dependendo do que você espera que um desses exemplos que fazer, ele pode conter uma surpresa.

#### Correspondência incorretamente contra uma variável

```haskell
-- file: ch03/BogusPattern.hs
data Fruit = Apple | Orange

apple = "apple"

orange = "orange"        

whichFruit :: String -> Fruit

whichFruit f = case f of
                 apple  -> Apple
                 orange -> Orange
```

Um olhar ingênuo sugere que este código está tentando verificar o valor de `f` para ver se corresponde ao valor `maçã` ou `laranja`.

É mais fácil detectar o erro se reescrever o código em um estilo equacional.

```haskell
-- file: ch03/BogusPattern.hs
equational apple = Apple
equational orange = Orange
```

Agora você pode ver o problema? Aqui, é mais óbvio `maçã` não se refere ao valor mais alto nível o nome `maçã`: é uma variável padrão local.

>![[Note]]({{site.url}}/rwh-ptbr/assets/note.png)**IPadrões irrefutáveis**

>Referimo-nos a um padrão que sempre sucede como _irrefutável_. Nomes de variáveis simples e um curinga `_` são exemplos de padrões irrefutáveis.

Aqui está uma versão corrigida da função.

```haskell
-- file: ch03/BogusPattern.hs
betterFruit f = case f of
                  "apple"  -> Apple
                  "orange" -> Orange
```
Nós fixamos o problema, fazer a comparação com os valores literal `"maçã"` e `"laranjas"`.

#### Tentando comparar a igualdade incorretamente

O que se pretende comparar os valores armazenados em dois nós da Árvore, do tipo, e retornar um deles se eles são iguais? Aqui está uma tentativa.

```haskell
-- file: ch03/BadTree.hs
bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
bad_nodesAreSame _            _            = Nothing
```

Um nome pode aparecer apenas uma vez em um conjunto de ligações padrão. Nós não podemos colocar uma variável em várias posições para expressar a noção de “este valor e que devem ser idênticos”. Em vez disso, nós vamos resolver esse problema utilizando _guardas_, outra característica Haskell inestimável.

### Avaliação condicional com guardas


Correspondência de padrões Limites nos a realizar testes de valor fixo de uma forma. Embora isso seja útil, nós, muitas vezes, querem fazer uma verificação mais expressivo antes de avaliar a função do órgão. Haskell oferece um recurso, os _guardas_, que nos dão esta habilidade. Vamos introduzir a idéia de uma modificação da função que escrevi para comparar dois nós de uma árvore.

```haskell
-- file: ch03/BadTree.hs
bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
bad_nodesAreSame _            _            = Nothing
```

Neste exemplo, usamos o padrão de correspondência para garantir que estamos olhando para os valores da forma correta, e um guarda para comparar peças deles.

Um padrão pode ser seguido por zero ou mais guardas, cada uma expressão do tipo Bool. A guarda é introduzida por um símbolo `|`. Isto é seguido pela expressão guarda, em seguida, um símbolo `=` (ou `->` se estamos em uma expressão `case`), então o organismo a usar a expressão guarda se avalia a `True`. Se um padrão casar, cada guarda associados a esse padrão é avaliada, na ordem em que são escritos. Se um guarda-sucedido, o corpo associada a ele é usado como o resultado da função. Se não for bem-sucedido de guarda, o padrão de correspondência se move para o padrão seguinte.

Quando um guarda de expressão é avaliada, de todas as variáveis mencionadas no padrão com o qual está associado é limitado e pode ser usado.

Aqui está uma versão melhorada da nossa função `emprestado` que usa as guardas.

```haskell
-- file: ch03/Lending.hs
lend3 amount balance
     | amount <= 0            = Nothing
     | amount > reserve * 0.5 = Nothing
     | otherwise              = Just newBalance
    where reserve    = 100
          newBalance = balance - amount
```

A procura de guarda-expressão especial `otherwise` é simplesmente uma variável associada ao valor `True`, para ajudar a legibilidade.

Podemos usar os guardas em qualquer lugar que nós podemos usar os padrões. Escrever uma função como uma série de equações usando a correspondência de padrão e os guardas podem torná-lo mais claro. Lembre-se da função `meuDrop` é definida na [seção chamada “Avaliação condicional”](types-and-functions.html#funcstypes.if "Conditional evaluation")?

```haskell
-- file: ch02/myDrop.hs
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)
```
Aqui está uma reformulação que utiliza padrões e guardas.

```haskell
-- file: ch02/myDrop.hs
niceDrop n xs | n <= 0 = xs
niceDrop _ []          = []
niceDrop n (_:xs)      = niceDrop (n - 1) xs
```
Esta mudança de estilo nos permite enumerar _se frente_ os casos em que esperamos uma função para se comportar de maneira diferente. Se enterrar as decisões dentro de uma função como expressões `if` o código se torna mais difícil de ler.

Exercícios
----------

**1.** Escreva uma função que calcula o número de elementos em uma lista. Para testá-lo, verifique se ele dá a mesma resposta que o função padrão `length`.

**2.** Adicionar uma assinatura para o seu tipo de função para o seu arquivo de origem. Para testá-lo, carregar o arquivo de origem em **ghci** novamente.

**3.** Escreva uma função que calcula a média de uma lista, ou seja, a soma de todos os elementos da lista dividida pelo seu comprimento. (Você pode precisar usar a função `fromIntegral` para converter o tamanho da lista de um número inteiro em um número de ponto flutuante).

**4.** Vire em uma lista um palíndromo, ou seja, deve ler-se o mesmo tanto de trás para frente. Por exemplo, dada a lista `[1,2,3]`, a sua função deve retornar `[1,2,3,3,2,1]`.

**5.** Escreva uma função que determina se a sua lista de entrada é um palíndromo.

**6.** Criar uma função que ordena uma lista de listas com base no comprimento de cada sublista. (Você pode querer olhar para a função `sortBy` da módulo `Data.List`.)

**7.** Definir uma função que se junta a uma lista de listas, juntamente com um valor de separação.

```haskell
-- file: ch03/Intersperse.hs
intersperse :: a -> [[a]] -> [a]
```

O separador deve aparecer entre os elementos da lista, mas não deve seguir o último elemento. Sua função deve se comportar como se segue.

	ghci> :load Intersperse
	[1 of 1] Compiling Main             ( Intersperse.hs, interpreted )
	Ok, modules loaded: Main.
	ghci> intersperse ',' []
	""
	ghci> intersperse ',' ["foo"]
	"foo"
	ghci> intersperse ',' ["foo","bar","baz","quux"]
	"foo,bar,baz,quux"

**8.** Usando o tipo de árvore binária que definimos anteriormente neste capítulo, escreva uma função que irá determinar a altura da árvore. A altura é o maior número de saltos a partir da raiz de um `Nada`. Por exemplo, a árvore `Nada` tem altura zero; `Nó "x" Nada Nada` tem altura de um; `Nó "x" Nada (Nó "y" Nada Nada)` tem altura de dois, e assim por diante.

**9.** Considere treis pontos 2D _a_, _b_ e _c_. Se olharmos para o ângulo formado pelo segmento de linha a partir de _a_ para _b_ e o segmento de linha de _b_ para _c_, que quer virar à esquerda, vire à direita, ou faz uma linha reta. Definir um tipo de dados Direção que permite representar essas possibilidades.

**10.** Escreva uma função que calcula a curva feita por três pontos 2D e retorna uma Direção.

**11.** Definir uma função que recebe uma lista de pontos 2D e calcula a direção de cada sucessivas triplo. Dada uma lista de pontos de `[a,b,c,d,e]`, ideve começar por calcular a vez feita por `[a,b,c]`, então a vez feita por `[b,c,d]`, então `[c,d,e]`. Sua função deve retornar uma lista de Direção.

**12.** Utilizando o código de nos últimos três exercícios, implementar Graham's scan algoritmo para o casco convexo de um conjunto de pontos 2D. Você pode encontrar uma boa descrição do que é um [envoltória convexa](http://pt.wikipedia.org/wiki/Envolt%C3%B3ria_convexa) e como o [exame de Graham](http://pt.wikipedia.org/wiki/Exame_de_Graham) deve trabalhar, na [Wikipedia](http://pt.wikipedia.org/).

  

* * *

\[[7](#id582956)\] Se você estiver familiarizado com C ou C++, é análogo a um `typedef`.

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart e John Goerzen. Esta obra está licenciada sob uma [Creative Commons Attribution-Noncommercial 3.0 License](http://creativecommons.org/licenses/by-nc/3.0/). Ícones por [Paul Davey](mailto:mattahan@gmail.com) aka [Mattahan](http://mattahan.deviantart.com/).

