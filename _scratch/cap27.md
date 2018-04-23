 (function(){var a="start";function b(e){this.t={};this.tick=function(f,g,c){this.t\[f\]=\[c?c:(new Date).getTime(),g\]};this.tick(a,null,e)}var d=new b;window.jstiming={Timer:b,load:d};try{var h=null;if(window.chrome&&window.chrome.csi)h=Math.floor(window.chrome.csi().pageT);if(h==null)if(window.gtbExternal)h=window.gtbExternal.pageT();if(h==null)if(window.external)h=window.external.pageT;if(h)window.jstiming.pt=h}catch(i){};})() var KX\_timer = new window.jstiming.Timer(); KX\_timer.name = 'published';Tradução do cpítulo 27 Sockets and syslog body { font-family: arial, sans, sans-serif; margin: 0; } iframe { border: 0; frameborder: 0; height: 100%; width: 100%; } #header, #footer { background: #f0f0f0; padding: 10px 10px; } #header { border-bottom: 1px #ccc solid; } #footer { border-top: 1px #ccc solid; border-bottom: 1px #ccc solid; font-size: 13; } #contents { margin: 6px; } .dash { padding: 0 6px; }

Tradução do cpítulo 27 Sockets and syslog

ol{margin:0;padding:0}p{margin:0}.c0{color:#45818e;font-style:italic;font-size:11pt;font-family:Arial;font-weight:bold}.c11{color:#38761d;font-style:italic;font-size:11pt;font-family:Arial}.c4{color:#45818e;font-style:italic;font-size:11pt;font-family:Arial}.c13{padding-left:0pt;line-height:1.15;direction:ltr;margin-left:36.0pt}.c5{color:#000000;font-size:18pt;font-family:Arial;font-weight:bold}.c2{color:#0000ff;font-style:italic;font-size:11pt;font-family:Arial}.c6{color:#bf9000;font-style:italic;font-size:11pt;font-family:Arial}.c1{color:#000000;font-size:11pt;font-family:Arial}.c12{padding-top:24.0pt;text-align:center;padding-bottom:6.0pt}.c9{color:#bf9000;font-size:11pt;font-family:Arial}.c10{color:#000000;font-size:24pt;font-family:Arial}.c17{color:#45818e;font-size:11pt;font-family:Arial}.c3{line-height:1.15;text-indent:0pt;direction:ltr}.c8{padding-top:18.0pt;padding-bottom:4.0pt}.c15{list-style-type:disc}.c7{font-weight:bold}.c16{font-style:italic}.c18{background-color:#ffffff}.c14{text-align:center}

Cezar T. Kavassaka-13094

Marcel Mendonça Grilo-13106

Júnio Salomé - 13105

Capítulo 27 Sockets e syslog

Básico de redes:

Nos capítulos anteriores deste livro discutimos sobre os serviços que operam em uma rede. Exemplos:

1.  Bancos de dados cliente / servidor
2.  web services.

Quando surge a necessidade de elaborar um novo protocolo, ou se comunicar com um protocolo que não possui uma biblioteca auxiliar existente em Haskell, você vai precisar usar as ferramentas de rede de baixo nível na biblioteca Haskell.

Neste capítulo, vamos discutir ferramentas de baixo nível. comunicação em rede é um tema amplo, com livros inteiros dedicados a ele. Vamos mostrar também, como usar Haskell para aplicaçar conhecimento  de baixo nível numa rede que já exsta.

As funções de rede em Haskell, quase sempre tem correspondência direta com as chamadas de função na linguagem C. Como a maioria das outras linguagens também tem camada em cima de C, você deve encontrar essa interface familiar.

Comunicando-se com UDP

UDP divide os dados em pacotes. Não garante que os dados chegam ao seu destino, ou chegam apenas uma vez. Ele faz uso de checksum para garantir que os pacotes que chegam, não estejam corrompidos. UDP tende a ser usado em aplicações que têm menos desempenho ou sejam sensíveis à latência, em que cada pacote individual de dados é menos importante do que o desempenho global do sistema. Também pode ser utilizado quando o comportamento do TCP não é o mais eficiente, como os que enviam mensagens curtas e discretas. Exemplos de sistemas que tendem a usar UDP incluem conferências de áudio e vídeo, sincronização de tempo, sistemas de arquivos baseados em rede e sistemas de registro.

Exemplo UDP Client: syslog

O serviço syslog tradicional no Unix permite que programas enviem mensagens de log através de uma rede para um servidor central que irá grava-los. Alguns programas são muito sensíveis ao desempenho, e podem gerar um grande volume de mensagens. Nestes programas, pode ser mais importante ter o registro e impor uma sobrecarga de desempenho mínimo para garantir que cada mensagem é registrada. Além disso, pode ser desejável para continuar a operação do programa, mesmo se o servidor de registro é inacessível. Por esta razão, o UDP é um dos protocolos suportados pelo syslog para a transmissão de mensagens de log. O protocolo é simples e nós apresentamos uma implementação de Haskell de um cliente.

\-\- file: ch27/syslogclient.hs

import Data.Bits

import Network.Socket

import Network.BSD

import Data.List

import SyslogTypes

data SyslogHandle =

  SyslogHandle {slSocket :: Socket,

                    slProgram :: String,

                    slAddress :: SockAddr}

openlog :: HostName                 -- ^ Remote hostname, or localhost

          -\> String                   -- ^ Port number or name; 514 is default

          -\> String                   -- ^ Name to log under

          -\> IO SyslogHandle          -- ^ Handle to use for logging

openlog hostname port progname =

  do -- Look up the hostname and port.  Either raises an exception

         \-\- or returns a nonempty list.  First element in that list

         \-\- is supposed to be the best option.

         addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)

         let serveraddr = head addrinfos

         \-\- Establish a socket for communication

         sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

         \-\- Save off the socket, program name, and server address in a handle

         return $ SyslogHandle sock progname (addrAddress serveraddr)

syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()

syslog syslogh fac pri msg =

  sendstr sendmsg

  where code = makeCode fac pri

            sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++

                      ": " \+\+ msg

            \-\- Send until everything is done

            sendstr :: String -> IO ()

            sendstr \[\] = return ()

            sendstr omsg = do sent <- sendTo (slSocket syslogh) omsg

                                      (slAddress syslogh)

                              sendstr (genericDrop sent omsg)

closelog :: SyslogHandle -> IO ()

closelog syslogh = sClose (slSocket syslogh)

{\- | Convert a facility and a priority into a syslog code -}

makeCode :: Facility -> Priority -> Int

makeCode fac pri =

  let faccode = codeOfFac fac

          pricode = fromEnum pri

          in

            (faccode \`shiftL\` 3) .|. pricode

Isto requer ainda um  SyslogTypes.hs, como segue:

\-\- file: ch27/SyslogTypes.hs  
module SyslogTypes where  
{\- | Priorities define how important a log message is. -}  
  
data Priority =  
           DEBUG                   -- ^ Debug messages  
         | INFO                    -- ^ Information  
         | NOTICE                  -- ^ Normal runtime conditions  
         | WARNING                 -- ^ General Warnings  
         | ERROR                   -- ^ General Errors  
         | CRITICAL                -- ^ Severe situations  
         | ALERT                   -- ^ Take immediate action  
         | EMERGENCY               -- ^ System is unusable  
                   deriving (Eq, Ord, Show, Read, Enum)  
  
{\- | Facilities are used by the system to determine where messages  
are sent. -}  
  
data Facility =  
             KERN                      -- ^ Kernel messages  
             | USER                    -- ^ General userland messages  
             | MAIL                    -- ^ E-Mail system  
             | DAEMON                  -- ^ Daemon (server process) messages  
             | AUTH                    -- ^ Authentication or security messages  
             | SYSLOG                  -- ^ Internal syslog messages  
             | LPR                     -- ^ Printer messages  
             | NEWS                    -- ^ Usenet news  
             | UUCP                    -- ^ UUCP messages  
             | CRON                    -- ^ Cron messages  
             | AUTHPRIV                -- ^ Private authentication messages  
             | FTP                     -- ^ FTP messages  
             | LOCAL0                    
             | LOCAL1  
             | LOCAL2  
             | LOCAL3  
             | LOCAL4  
             | LOCAL5  
             | LOCAL6  
             | LOCAL7  
               deriving (Eq, Show, Read)  
  
facToCode = \[  
                      (KERN, 0),  
                      (USER, 1),  
                      (MAIL, 2),  
                      (DAEMON, 3),  
                      (AUTH, 4),  
                      (SYSLOG, 5),  
                      (LPR, 6),  
                      (NEWS, 7),  
                      (UUCP, 8),  
                      (CRON, 9),  
                      (AUTHPRIV, 10),  
                      (FTP, 11),  
                      (LOCAL0, 16),  
                      (LOCAL1, 17),  
                      (LOCAL2, 18),  
                      (LOCAL3, 19),  
                      (LOCAL4, 20),  
                      (LOCAL5, 21),  
                      (LOCAL6, 22),  
                      (LOCAL7, 23)  
          \]  
  
codeToFac = map (\\(x, y) -> (y, x)) facToCode  
  
  
{\- | We can't use enum here because the numbering is discontiguous -}  
codeOfFac :: Facility -> Int  
codeOfFac f = case lookup f facToCode of  
               Just x -> x  
               _ -> error $ "Internal error in codeOfFac"  
  
facOfCode :: Int -> Facility  
facOfCode f = case lookup f codeToFac of  
               Just x -> x  
               _ -> error $ "Invalid code in facOfCode"

Com ghci, você pode enviar uma mensagem para um servidor syslog local. Você pode usar tanto o servidor syslog do exemplo apresentado neste capítulo, ou um servidor de syslog existentes, como você faria normalmente para encontrar no Linux ou outros sistemas POSIX. Note que a maioria destes desativam a porta UDP por padrão e pode ser necessário que se permita(ative) a porta UDP antes que seu fornecedor daemon do syslog exibia as mensagens recebidas.

Se você estiver enviando uma mensagem para um servidor syslog no sistema local, você pode usar um comando como segue:

ghci> :load syslogclient.hs\[1 of 2\] Compiling SyslogTypes      ( SyslogTypes.hs, interpreted )  
\[2 of 2\] Compiling Main             ( syslogclient.hs, interpreted )  
Ok, modules loaded: SyslogTypes, Main.  
ghci> h <- openlog "localhost" "514" "testprog"Loading package parsec-2.1.0.0 ... linking ... done.  
Loading package network-2.1.0.0 ... linking ... done.  
ghci> syslog h USER INFO "This is my message"ghci> closelog h

Servidor UDP  Syslog

Servidores UDP  se ligarão a uma porta específica na máquina servidora. Eles vão aceitar pacotes direcionados a essa porta e processá-los. Como UDP é um protocolo stateless e packet-oriented, os programadores utilizam normalmente uma chamada como recvfrom para receber tanto os dados quanto informações sobre a máquina que o enviou, e é usado para enviar de volta uma resposta.

\-\- file: ch27/syslogserver.hs  
import Data.Bits  
import Network.Socket  
import Network.BSD  
import Data.List  
  
type HandlerFunc = SockAddr -> String -> IO ()  
  
serveLog :: String              -- ^ Port number or name; 514 is default  
        -\> HandlerFunc         -- ^ Function to handle incoming messages  
        -\> IO ()  
serveLog port handlerfunc = withSocketsDo $  
   do -- Look up the port.  Either raises an exception or returns  
      \-\- a nonempty list.    
      addrinfos <- getAddrInfo  
                   (Just (defaultHints {addrFlags = \[AI_PASSIVE\]}))  
                   Nothing (Just port)  
      let serveraddr = head addrinfos  
  
      \-\- Create a socket  
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol  
  
      \-\- Bind it to the address we're listening to  
      bindSocket sock (addrAddress serveraddr)  
  
      \-\- Loop forever processing incoming data.  Ctrl-C to abort.  
      procMessages sock  
   where procMessages sock =  
             do -- Receive one UDP packet, maximum length 1024 bytes,  
                \-\- and save its content into msg and its source  
                \-\- IP and port into addr  
                (msg, _, addr) <- recvFrom sock 1024  
                \-\- Handle it  
                handlerfunc addr msg  
                \-\- And process more messages  
                procMessages sock  
  
\-\- A simple handler that prints incoming packets  
plainHandler :: HandlerFunc  
plainHandler addr msg =  
   putStrLn $ "From " ++ show addr ++ ": " ++ msg

Você pode executar no ghci. Uma chamada para serveLog "1514" plainHandler vai configurar um servidor UDP na porta 1514 que irá utilizar plainHandler para imprimir todos os pacotes UDP de entrada nessa porta. Ctrl-C irá encerrar o programa.

Em caso de problemas

Primeiros efeitos: permissão negada ao testar? Certifique-se de usar um número de porta maior que 1024. Alguns sistemas operacionais permitem que apenas o usuário root possa ligar em portas a menores de 1024.

Comunicando-se com TCP

O protocolo TCP foi projetado para fazer a transferência de dados através da Internet o mais confiável possível. O tráfego TCP é um fluxo de dados. Embora essa corrente é quebrada em pacotes individuais pelo sistema operacional, os limites dos pacotes não eram conhecidos nem relevantes para as aplicações. O protocolo TCP garante que, se o tráfego é entregue à aplicação de qualquer forma, que chegara intacto, inalterado, exatamente uma vez, e em ordem. Obviamente, as coisas tal como um fio quebrado podem resultar que o tráfego de dados seja quebrado, e nenhum protocolo pode superar essas limitações.

Isso traz algumas desvantagens em comparação com UDP. Primeiro de tudo, existem alguns pacotes que devem ser enviados no início da conversa TCP para estabelecer o link. Para conversas muito curtas, então, o UDP teria uma vantagem de desempenho. Além disso, o TCP se esforça muito para obter os dados. Se um lado de uma conversa tenta enviar dados para um lado remoto, mas não recebe uma confirmação de volta, serão periodicamente retransmitidos os dados por algum tempo antes de o protocolo desistir. Isso faz com que o TCP seja robusto no que se trata da perda de pacotes. No entanto, isso também significa que o TCP não é a melhor escolha em tempo real, ou seja, protocolos que envolvam coisas como o áudio ao vivo ou vídeo.

Manipulação de múltiplos fluxos TCP

Com o TCP, as conexões são stateful. Isso significa que há um "canal" lógico dedicado entre um cliente e servidor, ao invés de apenas atirar pacotes como em UDP. Isso torna as coisas fáceis para os desenvolvedores do lado cliente. Os aplicativos de servidor quase sempre irão querer ser capazes de lidar com mais de uma conexão TCP ao mesmo tempo. Como então fazer isso?

No lado do servidor, você primeiro cria um socket e liga a uma porta, assim como UDP. Em vez de ouvir várias vezes os dados a partir de qualquer localização, o loop principal será em torno de aceitar(accept) o convite. Cada vez que um cliente se conecta, o servidor do sistema operacional aloca um novo socket para ele. Portanto, temos a tomada principal, usado apenas para escutar as conexões de entrada, e nunca para transmitir dados. Temos também o potencial de múltiplos soquetes filhos para serem usado ao mesmo tempo, cada um correspondendo a uma conversa TCP lógica.

Em Haskell, normalmente você vai usar forkIO para criar um segmento separado e leve para lidar com cada conversa com um filho. Haskell tem uma implementação interna  eficiente é que executada muito bem.

Servidor TCP Syslog

Digamos que queremos implementar syslog usando TCP em vez de UDP. Poderíamos dizer que uma única mensagem não é definida por estar em um único pacote, mas é encerrada por um caractere de nova linha '\ n'. Um determinado cliente pode enviar 0 ou mais mensagens para o servidor usando uma determinada conexão TCP. Veja como podemos escrever isso.

\-\- file: ch27/syslogtcpserver.hs  
import Data.Bits  
import Network.Socket  
import Network.BSD  
import Data.List  
import Control.Concurrent  
import Control.Concurrent.MVar  
import System.IO  
  
type HandlerFunc = SockAddr -> String -> IO ()  
  
serveLog :: String              -- ^ Port number or name; 514 is default  
        -\> HandlerFunc         -- ^ Function to handle incoming messages  
        -\> IO ()  
serveLog port handlerfunc = withSocketsDo $  
   do -- Look up the port.  Either raises an exception or returns  
      \-\- a nonempty list.    
      addrinfos <- getAddrInfo  
                   (Just (defaultHints {addrFlags = \[AI_PASSIVE\]}))  
                   Nothing (Just port)  
      let serveraddr = head addrinfos  
  
      \-\- Create a socket  
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol  
  
      \-\- Bind it to the address we're listening to  
      bindSocket sock (addrAddress serveraddr)  
  
      \-\- Start listening for connection requests.  Maximum queue size  
      \-\- of 5 connection requests waiting to be accepted.  
      listen sock 5  
  
      \-\- Create a lock to use for synchronizing access to the handler  
      lock <- newMVar ()  
  
      \-\- Loop forever waiting for connections.  Ctrl-C to abort.  
      procRequests lock sock  
  
   where  
         \-\- | Process incoming connection requests  
         procRequests :: MVar () -> Socket -> IO ()  
         procRequests lock mastersock =  
             do (connsock, clientaddr) <- accept mastersock  
                handle lock clientaddr  
                   "syslogtcpserver.hs: client connnected"  
                forkIO $ procMessages lock connsock clientaddr  
                procRequests lock mastersock  
  
         \-\- | Process incoming messages  
         procMessages :: MVar () -> Socket -> SockAddr -> IO ()  
         procMessages lock connsock clientaddr =  
             do connhdl <- socketToHandle connsock ReadMode  
                hSetBuffering connhdl LineBuffering  
                messages <- hGetContents connhdl  
                mapM_ (handle lock clientaddr) (lines messages)  
                hClose connhdl  
                handle lock clientaddr  
                   "syslogtcpserver.hs: client disconnected"  
  
         \-\- Lock the handler before passing data to it.  
         handle :: MVar () -> HandlerFunc  
         \-\- This type is the same as  
         \-\- handle :: MVar () -> SockAddr -> String -> IO ()  
         handle lock clientaddr msg =  
             withMVar lock  
                (\\a -> handlerfunc clientaddr msg >> return a)  
  
\-\- A simple handler that prints incoming packets  
plainHandler :: HandlerFunc  
plainHandler addr msg =  
   putStrLn $ "From " ++ show addr ++ ": " ++ msg

Para a nossa implementação SyslogTypes, consulte a secção "Exemplo UDP Client: syslog"

Vamos olhar para este código. Nosso circuito principal é em procRequests, onde loop sempre espera por novas conexões dos clientes. A aceitação chama blocos até que um cliente se conecte. Quando um cliente liga(conecta), temos um novo socket e o endereço do cliente. Passamos uma mensagem para o manipulador sobre isso, então use forkIO criar uma thread para lidar com os dados desse cliente. Esta discussão é executado procMessages

Quando se trata de dados em TCP, muitas vezes é conveniente converter um socket em um handle no Haskell.O que fazemos aqui é definir explicitamente o buffer, um ponto importante para a comunicação TCP. Em seguida, montamos uma “leitura preguiçosa” do handler do socket. Para cada linha de entrada, passamos o handler para lidar com ela. Quando não há mais dados - porque o user remoto fechou o socket- temos uma mensagem sobre isso como saida.

Como estamos lidando com várias mensagens recebidas de uma vez, temos de garantir que não estamos escrevendo várias mensagens de uma só vez no manipulador(handler). Isso poderia resultar em saída ilegível. Nós usamos um bloqueio simples para serializar o acesso para o manipulador, e escrever um nhandler simples para lidarmos com a função.

Você pode testar isso com o cliente que vamos apresentar em seguida, ou você ainda pode usar o programa telnet para se conectar a esse servidor. Cada linha de texto que você enviar para ele será impresso na tela pelo servidor. Vamos testá-lo:

ghci> :load syslogtcpserver.hs\[1 of 1\] Compiling Main             ( syslogtcpserver.hs, interpreted )  
Ok, modules loaded: Main.  
ghci> serveLog "10514" plainHandler  
Loading package parsec-2.1.0.0 ... linking ... done.  
Loading package network-2.1.0.0 ... linking ... done.

Neste ponto, o servidor irá começar a escutar as conexões na porta 10514. Ele não irá aparecer para fazer nada até que um cliente se conecte. Nós poderíamos usar telnet para se conectar ao servidor:

~$ telnet localhost 10514  
Trying 127.0.0.1...  
Connected to localhost.  
Escape character is '^\]'.Test message^\]telnet> quit  
Connection closed.

Enquanto isso, no nosso terminal em execução, o servidor TCP, você verá algo como isto:

From 127.0.0.1:38790: syslogtcpserver.hs: client connnected  
From 127.0.0.1:38790: Test message  
From 127.0.0.1:38790: syslogtcpserver.hs: client disconnected

Isto mostra um cliente conectado a partir da porta 38.790 na máquina local (127.0.0.1). Depois de conectado, ele enviou uma mensagem, e desconectou. Quando você está agindo como um cliente TCP, o sistema operacional atribui uma porta não utilizada para você. Este número de porta normalmente será diferente a cada vez que você executar o programa.

TCP Client Syslog

Agora, vamos escrever um cliente para o nosso protocolo TCP syslog. Este cliente será semelhante ao cliente UDP, mas há algumas mudanças. Em primeiro lugar, uma vez que o TCP é um protocolo de streaming, podemos enviar dados através de um identificador em vez de utilizar as operações de tomada de baixo nível. Em segundo lugar, não precisamos mais armazenar o endereço de destino no SyslogHandle já que estaremos usando um connect para estabelecer a conexão TCP. Finalmente, temos uma maneira de saber onde termina uma mensagem e começa o próximo. Com o UDP, que foi fácil, porque cada mensagem era um pacote lógico discreto. Com o TCP, vamos usar o caractere de nova linha '\ n' como o marcador de fim-de-mensagem, embora isso não signifique que cada mensagem possa conter a nova linha. Aqui está o nosso código:

\-\- file: ch27/syslogtcpclient.hs  
import Data.Bits  
import Network.Socket  
import Network.BSD  
import Data.List  
import SyslogTypes  
import System.IO  
  
data SyslogHandle =  
   SyslogHandle {slHandle :: Handle,  
                 slProgram :: String}  
  
openlog :: HostName             -- ^ Remote hostname, or localhost  
       -\> String               -- ^ Port number or name; 514 is default  
       -\> String               -- ^ Name to log under  
       -\> IO SyslogHandle      -- ^ Handle to use for logging  
openlog hostname port progname =  
   do -- Look up the hostname and port.  Either raises an exception  
      \-\- or returns a nonempty list.  First element in that list  
      \-\- is supposed to be the best option.  
      addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)  
      let serveraddr = head addrinfos  
  
      \-\- Establish a socket for communication  
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol  
  
      \-\- Mark the socket for keep-alive handling since it may be idle  
      \-\- for long periods of time  
      setSocketOption sock KeepAlive 1  
  
      \-\- Connect to server  
      connect sock (addrAddress serveraddr)  
  
      \-\- Make a Handle out of it for convenience  
      h <- socketToHandle sock WriteMode  
  
      \-\- We're going to set buffering to BlockBuffering and then  
      \-\- explicitly call hFlush after each message, below, so that  
      \-\- messages get logged immediately  
      hSetBuffering h (BlockBuffering Nothing)  
       
      \-\- Save off the socket, program name, and server address in a handle  
      return $ SyslogHandle h progname  
  
syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()  
syslog syslogh fac pri msg =  
   do hPutStrLn (slHandle syslogh) sendmsg  
      \-\- Make sure that we send data immediately  
      hFlush (slHandle syslogh)  
   where code = makeCode fac pri  
         sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++  
                   ": " \+\+ msg  
  
closelog :: SyslogHandle -> IO ()  
closelog syslogh = hClose (slHandle syslogh)  
  
{\- | Convert a facility and a priority into a syslog code -}  
makeCode :: Facility -> Priority -> Int  
makeCode fac pri =  
   let faccode = codeOfFac fac  
       pricode = fromEnum pri  
       in  
         (faccode \`shiftL\` 3) .|. pricode

Nós podemos testá-lo sob ghci. Se você ainda tiver o servidor de mais cedo TCP em execução, a sessão pode parecer algo como isto:

ghci> :load syslogtcpclient.hsLoading package base ... linking ... done.  
\[1 of 2\] Compiling SyslogTypes      ( SyslogTypes.hs, interpreted )  
\[2 of 2\] Compiling Main             ( syslogtcpclient.hs, interpreted )  
Ok, modules loaded: Main, SyslogTypes.  
ghci> openlog "localhost" "10514" "tcptest"Loading package parsec-2.1.0.0 ... linking ... done.  
Loading package network-2.1.0.0 ... linking ... done.  
ghci> sl <- openlog "localhost" "10514" "tcptest"ghci> syslog sl USER INFO "This is my TCP message"ghci> syslog sl USER INFO "This is my TCP message again"ghci> closelog sl

Over on the server, you'll see something like this:

No servidor você verá algo como isto:

From 127.0.0.1:46319: syslogtcpserver.hs: client connnected  
From 127.0.0.1:46319: <9>tcptest: This is my TCP message  
From 127.0.0.1:46319: <9>tcptest: This is my TCP message again  
From 127.0.0.1:46319: syslogtcpserver.hs: client disconnected

O <9> é a prioridade e o código de instanciação que está sendo enviado junto, assim como foi com UDP.

[Editar esta página](https://docs.google.com/document/d/1uO2MU85rfD5waXHo0n2iVa0bai_NEQIVTwfmDL5xAMA/edit "Tradução do cpítulo 27 Sockets and syslog"), se você tiver permissão–Publicado por [Google Docs](https://docs.google.com/ "Learn more about Google Docs") –[Denunciar abuso](https://docs.google.com/abuse?id=1uO2MU85rfD5waXHo0n2iVa0bai_NEQIVTwfmDL5xAMA)–5Atualizado automaticamente a cada minutos

(function(){var g="",i="&srt=",l="&tran=",m="&p=s",n="&npn=1",o="&apa=1",r="&",s="start",t="_",u=".",v="=",w="http://csi.gstatic.com/csi",x="?v=3",y="&s=",z="kix",A="&action=",B="&it=",C=",",D="&rt="; if(window.jstiming){window.jstiming.a={};window.jstiming.c=1;var E=function(b,c,e){var a=b.t\[c\],h=b.t.start;if(a&&(h||e)){a=b.t\[c\]\[0\];h=e!=undefined?e:h\[0\];return a-h}},G=function(b,c,e){var a=g;if(window.jstiming.pt){a+=i+window.jstiming.pt;delete window.jstiming.pt}try{if(window.external&&window.external.tran)a+=l+window.external.tran;else if(window.gtbExternal&&window.gtbExternal.tran)a+=l+window.gtbExternal.tran();else if(window.chrome&&window.chrome.csi)a+=l+window.chrome.csi().tran}catch(h){}var d= window.chrome;if(d)if(d=d.loadTimes){if(d().wasFetchedViaSpdy)a+=m;if(d().wasNpnNegotiated)a+=n;if(d().wasAlternateProtocolAvailable)a+=o}if(b.b)a+=r+b.b;d=b.t;var F=d.start,p=\[\],j=\[\],f;for(f in d)if(f!=s)if(f.indexOf(t)!=0){var k=d\[f\]\[1\];if(k)d\[k\]&&j.push(f+u+E(b,f,d\[k\]\[0\]));else F&&p.push(f+u+E(b,f))}delete d.start;if(c)for(var q in c)a+=r+q+v+c\[q\];return\[e?e:w,x,y+(window.jstiming.sn||z)+A,b.name,j.length?B+j.join(C):g,g,a,D,p.join(C)\].join(g)};window.jstiming.report=function(b,c,e){b=G(b,c,e); if(!b)return g;c=new Image;var a=window.jstiming.c++;window.jstiming.a\[a\]=c;c.onload=c.onerror=function(){delete window.jstiming.a\[a\]};c.src=b;c=null;return b}};})() KX\_timer.tick('tl'); if (document.location.protocol == 'https:') {window.jstiming.report(KX\_timer, undefined , 'https://gg.google.com/csi');} else {window.jstiming.report(KX_timer);}
