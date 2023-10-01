Capítulo 22. Exemplo Extendido: Programando um Web Client

[Real World Haskell](/)by Bryan O'Sullivan, Don Stewart, and John Goerzen
-------------------------------------------------------------------------

Capítulo 22. Exemplo Extendido: Programando um Web Client

[Anterior](using-databases.html) 

 

 [Próximo](gui-programming-with-gtk-hs.html)

Capítulo 22. Exemplo Extendido: Programando um Web Client
---------------------------------------------------------

**Tabela de Conteúdos**

[Tipos Básicos](extended-example-web-client-programming.html#webclient.types)

[Banco de Dados](extended-example-web-client-programming.html#webclient.db)

[O Interpretador XML](extended-example-web-client-programming.html#webclient.parser)

[Download de Dados](extended-example-web-client-programming.html#webclient.download)

[Programa Principal](extended-example-web-client-programming.html#webclient.main)

A esta altura, você viu como interagir com uma base de dados, realizar analise sintática de tokens em arquivos, e manipular erros. Agora vamos dar um passo a frente e introduzir uma biblioteca de um web client à esta mistura.

Neste capítulo, vamos desenvolver uma aplicação real: um gerenciador de downloads de podcasts, ou podcatcher. A idéia por trás do podcatcher é simples. Ele processa uma lista de URLs dada. O download destas URLs resulta em um arquivo XML no formato RSS. Dentro deste arquivo XML, encontram-se referências para o download de arquivos de áudio.

O podcatcher realmente possibilita a um usuário que assine podcasts, adicionando URLs ao arquivo de configuração. Desta forma, o usuário pode periodicamente rodar um update. O podcatcher vai realizar o download dos arquivos RSS, examiná-los buscando por referências de arquivos de áudio, e baixar os arquivos de áudio que ainda não tenham sido transferidos em nome deste usuário.

![[Tip]](/support/figs/tip.png)

Dica

Os usuários costumam chamar o arquivo RSS de podcast ou feed do podcast, e cada arquivo de áudio de episódio.

Para que isso aconteça, necessitamos de algumas coisas:

*   uma biblioteca HTTP client para realizar o download dos arquivos
    
*   um interpretador XML
    
*   uma forma de especificar e armazenar quais podcasts que estamos interessados
    
*   uma forma de armazenar quais podcasts já foram baixados
    

Os dois últimos itens podem ser concretizados utlizando-se de uma base de dados que vamos configurar utilizando HDBC.

![[Tip]](/support/figs/tip.png)

Dica

O código contido neste capítulo foi escrito especificamente para os propósitos deste livro, mas é baseado no código escrito para hpodder, um podcatcher escrito em Haskell. Hpodder possui muitos mais recursos que os exemplos aqui apresentados, transformando-o em um exemplo muito extenso e complexo para os propósitos deste livro. Se você está interessado em estudar mais sobre o hpodder, seu código fonte está livremente disponível em [http://software.complet e.org/hpodder](http://software.complete.org/hpodder).

Vamos escrever o código deste capítulo em pedaços. Cada trecho de código será seu próprio módulo Haskell. Você será capaz de brincar com cada trecho de código no **ghci**. Ao final, teremos escrito todo código que junto reúne os requisitos da aplicação. Vamos começar com os tipos básicos que iremos utilizar.

Tipos Básicos
-------------

A primeira coisa a fazer é identificar qual é a informação básica relevante ao aplicativo. Geralmente esta informação diz respeito aos podcasts que o usuário está interessado, além da informação sobre os episódios que já foram vistos e processados. É facil alterar tais dados posteriormente se necessário, mas já que vamos utilizá-los em todos os lugares do aplicativo, devemos primeiramente defini-los.

\-\- file: ch22/PodTypes.hs
module PodTypes where

data Podcast =
    Podcast {castId :: Integer, -- ^ Numeric ID for this podcast
             castURL :: String  -- ^ Its feed URL
            }
    deriving (Eq, Show, Read)

data Episode = 
    Episode {epId :: Integer,     -- ^ Numeric ID for this episode
             epCast :: Podcast, -- ^ The ID of the podcast it came from
             epURL :: String,     -- ^ The download URL for this episode
             epDone :: Bool       -- ^ Whether or not we are done with this ep
            }
    deriving (Eq, Show, Read)

Nós iremos armazenar esta informação em uma base de dados. Devemos manter apenas um identificador único tanto para o podcast quanto para o episódio, pois isto facilita o trabalho de descobrir quais episódios pertencem a um determinado podcast, o carregamento de um episódio ou podcast específico ou ainda tratar possíveis problemas no futuro, como a mudança do endereço da URL do podcast.

Banco de Dados
--------------

A seguir, vamos escrever o código para persistir os dados na base de dados. Primeiramente, vamos atentar a forma como mover dados entre as estruturas Haskell que definimos em `PodTypes.hs` e a base de dados. Além disso, a primeira vez que o usuário executa o programa, necessitamos criar as tabelas da base de dados que vamos utilizar para persistir os dados.

Faremos uso do HDBC (veja [Capítulo 21, _Utilizando Base de Dados_](using-databases.html "Capítulo 21, Utilizando Base de Dados")) para interagir com uma base de dados Sqlite. Sqlite é uma ferramenta leve e auto-suficiente, o que a torna perfeita para este projeto. Para maiores informações acerca da instalação do HDBC e do Sqlite consulte [a seção "Instalando HDBC e Drivers".”](using-databases.html#databases.hdbc.install "Instalando HDBC e Drivers").

\-\- file: ch22/PodDB.hs
module PodDB where

import Database.HDBC
import Database.HDBC.Sqlite3
import PodTypes
import Control.Monad(when)
import Data.List(sort)

\-\- | Initialize DB and return database Connection
connect :: FilePath -> IO Connection
connect fp =
    do dbh <- connectSqlite3 fp
       prepDB dbh
       return dbh

{\- | Prepare the database for our data.

We create two tables and ask the database engine to verify some pieces
of data consistency for us:

\* castid and epid both are unique primary keys and must never be duplicated
\* castURL also is unique
\* In the episodes table, for a given podcast (epcast), there must be only
  one instance of each given URL or episode ID
-}
prepDB :: IConnection conn => conn -> IO ()
prepDB dbh =
    do tables <- getTables dbh
       when (not ("podcasts" \`elem\` tables)) $
           do run dbh "CREATE TABLE podcasts (\
                       \\castid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \\castURL TEXT NOT NULL UNIQUE)" \[\]
              return ()
       when (not ("episodes" \`elem\` tables)) $
           do run dbh "CREATE TABLE episodes (\
                       \\epid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \\epcastid INTEGER NOT NULL,\
                       \\epurl TEXT NOT NULL,\
                       \\epdone INTEGER NOT NULL,\
                       \\UNIQUE(epcastid, epurl),\
                       \\UNIQUE(epcastid, epid))" \[\]
              return ()
       commit dbh

{\- | Adds a new podcast to the database.  Ignores the castid on the
incoming podcast, and returns a new object with the castid populated.

An attempt to add a podcast that already exists is an error. -}
addPodcast :: IConnection conn => conn -> Podcast -> IO Podcast
addPodcast dbh podcast = 
    handleSql errorHandler $
      do -- Insert the castURL into the table.  The database
         \-\- will automatically assign a cast ID.
         run dbh "INSERT INTO podcasts (castURL) VALUES (?)"
             \[toSql (castURL podcast)\]
         \-\- Find out the castID for the URL we just added.
         r <- quickQuery' dbh "SELECT castid FROM podcasts WHERE castURL = ?"
              \[toSql (castURL podcast)\]
         case r of
           \[\[x\]\] -> return $ podcast {castId = fromSql x}
           y -> fail $ "addPodcast: unexpected result: " ++ show y
    where errorHandler e = 
              do fail $ "Error adding podcast; does this URL already exist?\\n"
                     \+\+ show e

{\- | Adds a new episode to the database. 

Since this is done by automation, instead of by user request, we will
simply ignore requests to add duplicate episodes.  This way, when we are
processing a feed, each URL encountered can be fed to this function,
without having to first look it up in the DB.

Also, we generally won't care about the new ID here, so don't bother
fetching it. -}
addEpisode :: IConnection conn => conn -> Episode -> IO ()
addEpisode dbh ep =
    run dbh "INSERT OR IGNORE INTO episodes (epCastId, epURL, epDone) \
                \\VALUES (?, ?, ?)"
                \[toSql (castId . epCast $ ep), toSql (epURL ep),
                 toSql (epDone ep)\]
    >\> return ()
       
{\- | Modifies an existing podcast.  Looks up the given podcast by
ID and modifies the database record to match the passed Podcast. -}
updatePodcast :: IConnection conn => conn -> Podcast -> IO ()
updatePodcast dbh podcast =
    run dbh "UPDATE podcasts SET castURL = ? WHERE castId = ?" 
            \[toSql (castURL podcast), toSql (castId podcast)\]
    >\> return ()

{\- | Modifies an existing episode.  Looks it up by ID and modifies the
database record to match the given episode. -}
updateEpisode :: IConnection conn => conn -> Episode -> IO ()
updateEpisode dbh episode =
    run dbh "UPDATE episodes SET epCastId = ?, epURL = ?, epDone = ? \
             \\WHERE epId = ?"
             \[toSql (castId . epCast $ episode),
              toSql (epURL episode),
              toSql (epDone episode),
              toSql (epId episode)\]
    >\> return ()

{\- | Remove a podcast.  First removes any episodes that may exist
for this podcast. -}
removePodcast :: IConnection conn => conn -> Podcast -> IO ()
removePodcast dbh podcast =
    do run dbh "DELETE FROM episodes WHERE epcastid = ?" 
         \[toSql (castId podcast)\]
       run dbh "DELETE FROM podcasts WHERE castid = ?"
         \[toSql (castId podcast)\]
       return ()

{\- | Gets a list of all podcasts. -}
getPodcasts :: IConnection conn => conn -> IO \[Podcast\]
getPodcasts dbh =
    do res <- quickQuery' dbh 
              "SELECT castid, casturl FROM podcasts ORDER BY castid" \[\]
       return (map convPodcastRow res)

{\- | Get a particular podcast.  Nothing if the ID doesn't match, or
Just Podcast if it does. -}
getPodcast :: IConnection conn => conn -> Integer -> IO (Maybe Podcast)
getPodcast dbh wantedId =
    do res <- quickQuery' dbh 
              "SELECT castid, casturl FROM podcasts WHERE castid = ?"
              \[toSql wantedId\]
       case res of
         \[x\] -> return (Just (convPodcastRow x))
         \[\] -\> return Nothing
         x -> fail $ "Really bad error; more than one podcast with ID"

{\- | Convert the result of a SELECT into a Podcast record -}
convPodcastRow :: \[SqlValue\] -> Podcast
convPodcastRow \[svId, svURL\] =
    Podcast {castId = fromSql svId,
             castURL = fromSql svURL}
convPodcastRow x = error $ "Can't convert podcast row " ++ show x

{\- | Get all episodes for a particular podcast. -}
getPodcastEpisodes :: IConnection conn => conn -> Podcast -> IO \[Episode\]
getPodcastEpisodes dbh pc =
    do r <- quickQuery' dbh
            "SELECT epId, epURL, epDone FROM episodes WHERE epCastId = ?"
            \[toSql (castId pc)\]
       return (map convEpisodeRow r)
    where convEpisodeRow \[svId, svURL, svDone\] =
              Episode {epId = fromSql svId, epURL = fromSql svURL,
                       epDone = fromSql svDone, epCast = pc}

No módulo `PodDB`, definimos as funções de conexão com a base de dados, criação das tabelas necessárias e de adicão, remoção e busca dos dados na base. Aqui encontra-se uma sessão **ghci** demonstrando as interação com a base de dados. Esta seção irá criar uma base de dados chamada `poddbtest.db` e adiciona um podcast e um episódio ao mesmo.

    ghci> 

O Interpretador XML
-------------------

Agora que já possuimos uma base de dados, necessitamos de um código que seja responsável por realizar a analise sintática dos podcasts. Tratam-se de arquivos XML que contém diversas informações. Aqui temos um arquivo que demonstra algumas dessas informações.

<?xml version="1.0" encoding="UTF-8"?>
<rss xmlns:itunes="http://www.itunes.com/DTDs/Podcast-1.0.dtd" version="2.0">
  <channel>
    <title>Haskell Radio</title>
    <link>http://www.example.com/radio/</link>
    <description>Description of this podcast</description>
    <item>
      <title>Episode 2: Lambdas</title>
      <link>http://www.example.com/radio/lambdas</link>
      <enclosure url="http://www.example.com/radio/lambdas.mp3"
       type="audio/mpeg" length="10485760"/>
    </item>
    <item>
      <title>Episode 1: Parsec</title>
      <link>http://www.example.com/radio/parsec</link>
      <enclosure url="http://www.example.com/radio/parsec.mp3"
       type="audio/mpeg" length="10485150"/>
    </item>
  </channel>
</rss>

Dentro do contexto destes arquivos, estamos especialmente interessados em duas coisas: no título do podcast e nas tags que contém o endereço das URLs dos episódios. Utilizamos da [ferramenta HaXml](http://www.cs.york.ac.uk/fp/HaXml/) para analisar este arquivo. A seguir temos o trecho de código referente a este módulo:

\-\- file: ch22/PodParser.hs
module PodParser where

import PodTypes
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Html.Generate(showattr)
import Data.Char
import Data.List

data PodItem = PodItem {itemtitle :: String,
                  enclosureurl :: String
                  }
          deriving (Eq, Show, Read)

data Feed = Feed {channeltitle :: String,
                  items :: \[PodItem\]}
            deriving (Eq, Show, Read)

{\- | Given a podcast and an PodItem, produce an Episode -}
item2ep :: Podcast -> PodItem -> Episode
item2ep pc item =
    Episode {epId = 0,
             epCast = pc,
             epURL = enclosureurl item,
             epDone = False}

{\- | Parse the data from a given string, with the given name to use
in error messages. -}
parse :: String -> String -> Feed
parse content name = 
    Feed {channeltitle = getTitle doc,
          items = getEnclosures doc}

    where parseResult = xmlParse name (stripUnicodeBOM content)
          doc = getContent parseResult

          getContent :: Document -> Content
          getContent (Document _ _ e _) = CElem e
          
          {\- | Some Unicode documents begin with a binary sequence;
             strip it off before processing. -}
          stripUnicodeBOM :: String -> String
          stripUnicodeBOM ('\\xef':'\\xbb':'\\xbf':x) = x
          stripUnicodeBOM x = x

{\- | Pull out the channel part of the document.

Note that HaXml defines CFilter as:

\> type CFilter = Content -> \[Content\]
-}
channel :: CFilter
channel = tag "rss" /> tag "channel"

getTitle :: Content -> String
getTitle doc =
    contentToStringDefault "Untitled Podcast" 
        (channel /> tag "title" /> txt $ doc)

getEnclosures :: Content -> \[PodItem\]
getEnclosures doc =
    concatMap procPodItem $ getPodItems doc
    where procPodItem :: Content -> \[PodItem\]
          procPodItem item = concatMap (procEnclosure title) enclosure
              where title = contentToStringDefault "Untitled Episode"
                               (keep /> tag "title" /> txt $ item)
                    enclosure = (keep /> tag "enclosure") item

          getPodItems :: CFilter
          getPodItems = channel /> tag "item"

          procEnclosure :: String -> Content -> \[PodItem\]
          procEnclosure title enclosure =
              map makePodItem (showattr "url" enclosure)
              where makePodItem :: Content -> PodItem
                    makePodItem x = PodItem {itemtitle = title,
                                       enclosureurl = contentToString \[x\]}

{\- | Convert \[Content\] to a printable String, with a default if the 
passed-in \[Content\] is \[\], signifying a lack of a match. -}
contentToStringDefault :: String -> \[Content\] -> String
contentToStringDefault msg \[\] = msg
contentToStringDefault _ x = contentToString x

{\- | Convert \[Content\] to a printable string, taking care to unescape it.

An implementation without unescaping would simply be:

\> contentToString = concatMap (show . content)

Because HaXml's unescaping only works on Elements, we must make sure that
whatever Content we have is wrapped in an Element, then use txt to
pull the insides back out. -}
contentToString :: \[Content\] -> String
contentToString = 
    concatMap procContent
    where procContent x = 
              verbatim $ keep /> txt $ CElem (unesc (fakeElem x))

          fakeElem :: Content -> Element
          fakeElem x = Elem "fake" \[\] \[x\]

          unesc :: Element -> Element
          unesc = xmlUnEscape stdXmlEscaper

Vamos dar uma olhada neste código. Primeiramente, definimos dois tipos: `PodItem` e `Feed`. Vamos transformar o arquivo XML em um `Feed`, que irá conter um ou mais itens (PodItem). Nós também definimos uma função para conversão do `PodItem` em um `Episode`, definido anteriormente em `PodTypes.hs`.

A seguir, realiza-se a análise. A função `parse` pega uma `String` que representa o conteúdo do arquivo XML bem como uma outra `String` que representará as mensagens de erro, e retorna um `Feed`.

HaXml é designado como um filtro para conversão dos dados do arquivo XML em outro formato. Esta pode ser uma simples conversão de XML para XML, bem como de XML para dados Haskell ou de Haskell para XML. HaXml possui um tipo de dado designado `CFilter`, que é definido como:

type CFilter = Content -> \[Content\]
    

Isto é, um tipo `CFilter` pega um fragmento de um arquivo XML e retorna nenhum ou muitos fragmentos. O tipo `CFilter` pode ser solicitado a encontrar todos os filhos de uma tag específica, ou todas as tags com um determinado nome, o texto literal contido em um determinado trecho de um arquivo XML, etc. Existe inclusive um operador `(/>)` que mantém as funções `CFilter` agrupadas. Todos os dados que nos dizem respeito podem ser encontrados entre a tag `<channel>`, portanto necessitamos obter tais dados. Para isto, definimos um simples `CFilter`:

channel = tag "rss" /> tag "channel"
    

Quando passamos um documento para encontrar o `channel` ele irá procurar de cima para baixo pela tag `rss`. Posteriormente, de posse disto, será procurada a tag `channel`.

O restante do programa segue este esquema básico de extração de literais em um arquivo de texto através de uma tag, e utilizando-se das funções do tipo `CFilter`, podemos obter tais informações em qualquer trecho do documento.

Download de Dados
-----------------

O próximo módulo de nosso programa será responsável por realizar download de dados. Necessitaremos de baixar dois tipos diferentes de dados: o conteúdo do podcast, e o áudio referente a cada episódio. No primeiro caso, vamos necessitar analisar os dados e atualizar nossa base de dados. Posteriormente, iremos armazenar os arquivos de áudio no disco.

Iremos realizar os downloads a partir de servidores HTTP, portanto necessitaremos fazer uso da [biblioteca HTTP](http://www.haskell.org/http/) embutida na linguagem Haskell. Para realizar o download dos arquivos de feed dos podcasts, necessitaremos de baixar o arquivo, analisá-lo e atualizar a base de dados. Para episódios, vamos baixar o arquivo, escrevê-lo no disco e marcá-lo como baixado na base de dados. A seguir temos o referente trecho de código:

\-\- file: ch22/PodDownload.hs
module PodDownload where
import PodTypes
import PodDB
import PodParser
import Network.HTTP
import System.IO
import Database.HDBC
import Data.Maybe
import Network.URI

{\- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success. -}
downloadURL :: String -> IO (Either String String)
downloadURL url =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ Right (rspBody r)
               (3,_,_) -> -- A HTTP redirect
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just url -> downloadURL url
               _ -> return $ Left (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = \[\],
                             rqBody = ""}
          uri = fromJust $ parseURI url

{\- | Update the podcast in the database. -}
updatePodcastFromFeed :: IConnection conn => conn -> Podcast -> IO ()
updatePodcastFromFeed dbh pc =
    do resp <- downloadURL (castURL pc)
       case resp of
         Left x -> putStrLn x
         Right doc -> updateDB doc

    where updateDB doc = 
              do mapM_ (addEpisode dbh) episodes
                 commit dbh
              where feed = parse doc (castURL pc)
                    episodes = map (item2ep pc) (items feed)

{\- | Downloads an episode, returning a String representing
the filename it was placed into, or Nothing on error. -}
getEpisode :: IConnection conn => conn -> Episode -> IO (Maybe String)
getEpisode dbh ep =
    do resp <- downloadURL (epURL ep)
       case resp of
         Left x -> do putStrLn x
                      return Nothing
         Right doc -> 
             do file <- openBinaryFile filename WriteMode
                hPutStr file doc
                hClose file
                updateEpisode dbh (ep {epDone = True})
                commit dbh
                return (Just filename)
          \-\- This function ought to apply an extension based on the filetype
    where filename = "pod." ++ (show . castId . epCast $ ep) ++ "." ++ 
                     (show (epId ep)) ++ ".mp3"

Este módulo define portanto três funções básicas: `downloadURL`, que simplesmente realiza o download de uma URL e a retorna como uma `String`; `updatePodcastFromFeed`, que faz o download do arquivo XML, analisa-o e atualiza seus dados na base de dados; `getEpisode`, que faz o download algum episódio solicitado pelo usuário e atualiza seu status como baixado na base de dados.

![[Warning]](/support/figs/warning.png)

Atenção

A biblioteca HTTP aqui utilizada não realiza a leitura dos resultados HTTP de forma preguiçosa. Como consequência, este fato pode resultar em um consumo de uma grande quantidade de memória RAM quando realizar o download de arquivos grandes, como podcasts. Existem outras bibliotecas disponíveis que não possuem esta limitação. Utilizamos este, pois trata-se de uma opção estável, de fácil instalação e uso. Sugerimos a utilização de mini-http, disponível no Hackage, para reais necessidade de uso HTTP.

Programa Principal
------------------

Finalmente, necessitamos de um programa principal para unir todas estas funcionalidades. A seguir temos nosso módulo principal:

\-\- file: ch22/PodMain.hs
module Main where

import PodDownload
import PodDB
import PodTypes
import System.Environment
import Database.HDBC
import Network.Socket(withSocketsDo)

main = withSocketsDo $ handleSqlError $
    do args <- getArgs
       dbh <- connect "pod.db"
       case args of
         \["add", url\] -> add dbh url
         \["update"\] -> update dbh
         \["download"\] -> download dbh
         \["fetch"\] -> do update dbh
                         download dbh
         _ -> syntaxError
       disconnect dbh

add dbh url = 
    do addPodcast dbh pc
       commit dbh
    where pc = Podcast {castId = 0, castURL = url}

update dbh = 
    do pclist <- getPodcasts dbh
       mapM_ procPodcast pclist
    where procPodcast pc =
              do putStrLn $ "Updating from " ++ (castURL pc)
                 updatePodcastFromFeed dbh pc

download dbh =
    do pclist <- getPodcasts dbh
       mapM_ procPodcast pclist
    where procPodcast pc =
              do putStrLn $ "Considering " ++ (castURL pc)
                 episodelist <- getPodcastEpisodes dbh pc
                 let dleps = filter (\\ep -> epDone ep == False)
                             episodelist
                 mapM_ procEpisode dleps
          procEpisode ep =
              do putStrLn $ "Downloading " ++ (epURL ep)
                 getEpisode dbh ep

syntaxError = putStrLn 
  "Usage: pod command \[args\]\\n\
  \\\n\
  \\pod add url      Adds a new podcast with the given URL\\n\
  \\pod download     Downloads all pending episodes\\n\
  \\pod fetch        Updates, then downloads\\n\
  \\pod update       Downloads podcast feeds, looks for new episodes\\n"

Temos um analisador de linha de comando bastante simples com uma função que indica erros na linha de comando, além de pequenas funções que tratam os diferentes argumentos da linha de comando.

Você pode compilar este programa com um comando como:

ghc --make -O2 -o pod -package HTTP -package HaXml -package network \
    -package HDBC -package HDBC-sqlite3 PodMain.hs
    

Alternativamente, você pode usar um arquivo Cabal como documentado na [seção chamada "Criando um pacote"](writing-a-library-working-with-json-data.html#library.package "Criando um pacote") para gerar este projeto:

\-\- ch23/pod.cabal
Name: pod
Version: 1.0.0
Build-type: Simple
Build-Depends: HTTP, HaXml, network, HDBC, HDBC-sqlite3, base

Executable: pod
Main-Is: PodMain.hs
GHC-Options: -O2
    

Além disso, você necessita de um arquivo `Setup.hs`:

import Distribution.Simple
main = defaultMain
    

Agora para gerar com Cabal, você deve rodar:

runghc Setup.hs configure
runghc Setup.hs build
    

E você encontrará um diretório `dist` contendo a saída. Para instalar o programa no sistema, rode o comando `runghc Setup.hs install`.

![](/support/figs/rss.png) Want to stay up to date? Subscribe to the comment feed for [this chapter](/feeds/comments/), or the [entire book](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen. This work is licensed under a [Creative Commons Attribution-Noncommercial 3.0 License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul Davey](mailto:mattahan@gmail.com) aka [Mattahan](http://mattahan.deviantart.com/).

[Anterior](using-databases.html) 

 

 [Próximo](gui-programming-with-gtk-hs.html)

Capítulo 21. Utilizando Banco de Dados

[Início](index.html)

 Capítulo 23. Progrmamando Interfaces Gráficas com GUI gtk2hs

_uacct = "UA-1805907-3"; urchinTracker();
