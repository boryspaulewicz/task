#!/usr/local/lib/R/bin/Rscript

## TODO: zapis danych ma ewentualnie poszerzać tabelę, jeżeli zachodzi
## taka potrzeba

######################################################################
## Struktura bazy danych
##
## Tabela z danymi <TASK_NAME>_data: id, age, gender, ... dla każdego
## zadania, a więc TASK.NAME musi być unikalne dla
## procedury.
##
## Tabela sesji session: task, id, cnd, timestamp, stage
## to 'started', albo 'finished' - nie każdemu udaje się skończyć.


## Zmienne globalne zapisujemy dużymi literami
##
## Komunikacja z bazą danych za pomocą skryptu php na serwerze, przez port HTTP
##
## Odrębne części kodu zaczynają się od potrójnego znaku komentarza
##
## Zdarzenia, w tym błędy, zapisywane w /taskdata/task.log
##
## Błąd wywułuje komunikat przez okno dialogowe i ewentualne wyjście z programu
##

## Nowa, uproszczona wersja - zapisuje dane w postaci tabel id, age,
## gender, ..., gdzie nazwa tabeli odpowiada nazwie zadania

## Uruchomienie na szybko:
##
## run.trials(trial.code, expand.grid(size = c(.1, .2), intensity = c(.2, .6, 1)))

library(sfml)
library(RGtk2)
library(httr)
library(XML) ## content tego potrzebuje
library(stringr)
library(compiler)
enableJIT(3)

######################################################################
### Zmienne globalne

## TASK.ID = 1
TASK.NAME = "template"
## SESSION.ID = NULL
## ADMIN.ID = 0
CHOSEN.BUTTON = ""
USER.DATA = list()
TASK.START = NULL
DB.IP = NULL
DB.DEBUG = FALSE

######################################################################
### Logi

task.log = function(log){
    cat(paste(date(), '\n', log, '\n', sep = ''), file = '/taskdata/task.log',
        append = T)
}

######################################################################
### Baza danych

## jako ... można quit.after = F
db.query = function(q, ip = DB.IP, ...){
    if(DB.DEBUG)print(q)
    res = GET(paste("http://", ip, "/task/query.php", sep = ''),
              query = list(do = q))
    if(status_code(res) != 200)gui.error.msg(paste("Nieudana próba połączenia z bazą danych.\nTreść zapytania:", q), ...)
    res
}

db.query.csv = function(q, ...){
    res = db.query(q, ...)
    text = content(res, "text")
    if(str_trim(text) != ""){
        read.csv(textConnection(text), stringsAsFactors = F)
    }else{ NULL }
}

## data to lista nazwanych wartości, table to nazwa tabeli
db.insert.query = function(data, table){
    nms = vls = "("
    for(i in 1:(length(data))){
        last.p = (i == length(data))
        numeric.p = is.numeric(data[[i]])
        nms = paste(nms, names(data)[i], ifelse(last.p, ")", ", "), sep = "")
        vls = paste(vls, ifelse(numeric.p, "", '"'), data[[i]], ifelse(numeric.p, "", '"'),
                    ifelse(last.p, ")", ", "), sep = "")
    }
    sprintf("insert into %s %s values %s;", table, nms, vls)
}

## IP bazy danych
db.ip = function(){
    l = try(readLines('/taskdata/ip'), T)
    if(class(l) == 'try-error'){
        gui.error.msg("Nie udało się ustalić adresu IP bazy danych")
    }else{
        str_trim(l[1])
    }
}

## ######################################################################
## ### Rejstracja sesji, zapis danych

## db.registered.session = function(task.name = TASK.NAME, condition = 'undefined', allow.new.id = F, allow.new.session = T,
##                                  oneshot = 1){
##     if(length(USER.DATA) < 3){
##         gui.error.msg("Nie podano wymaganych danych osobowych", quit.after = F)
##         return(0)
##     }
##     ## admin to specjalny użytkownik, który bierze udział w sesji testowej (id -1)
##     if(USER.DATA$name == 'admin')return(-1)
##     task.id = db.query.csv(sprintf('select id from project where name = \"%s\"', task.name))$id
##     if(length(task.id) == 0){
##         gui.error.msg(sprintf("Nie udało się znaleźć w bazie zadania o nazwie %s", task.name), quit.after = T)
##         return(0)
##     }
##     id.query = sprintf('select id from participant where name = \"%s\"', USER.DATA$name)
##     id = db.query.csv(id.query)$id
##     if(length(id) == 0){
##         if(allow.new.id){
##             db.query(sprintf('insert into participant (name, gender, age) values (\"%s\", \"%s\", %s)',
##                              USER.DATA$name, USER.DATA$gender, USER.DATA$age))
##             id = db.query.csv(id.query)$id
##         }else{
##             gui.error.msg("Nie znaleziono osoby badanej w bazie danych", quit.after = F)
##             return(0)
##         }
##     }else if(length(id) == 1){
##         ## Uaktualniamy dane osobowe
##         db.query(sprintf('update participant set gender = \"%s\", age = %s where id = %s', USER.DATA$gender, USER.DATA$age, id))
##     }else{
##         gui.error.msg(sprintf("Ta osoba występuje więcej niż raz w bazie danych: %s", USER.DATA), quit.after = F)
##         return(0)
##     }
##     session = db.query.csv(sprintf("select * from session where participant = %s and project = %s", id, task.id))
##     if(nrow(session) == 0){
##         if(allow.new.session){
##             db.query.csv(db.insert.query(list(participant = id, project = task.id, cnd = condition,
##                                           admin = ADMIN.ID, oneshot = oneshot, completed = 0), 'session'))
##             db.query.csv(sprintf("select id from session where participant = %s and project = %s;", id, task.id))$id
##         }else{
##             gui.error.msg("Brak upoważnienia do wykonania tego zadania", quit.after = F)
##             return(0)
##         }
##     }else if(nrow(session) == 1){
##         if((session$oneshot == 1) && (session$completed == 1)){
##             gui.error.msg("To zadanie można wykonać tylko raz", quit.after = F)
##             return(0)
##         }else{
##             return(session$id)
##         }
##     }else{
##         gui.error.msg("Więcej niż jedna zarejestrowana sesja badania", quit.after = F)
##         return(0)
##     }
## }

## Zwraca listę warunków wykonanych do tej pory w ramach sesji tego zadania
db.session.condition = function(task.name = TASK.NAME)db.query.csv(sprintf("select cnd from session where task = %s", task.name))$cnd

## Wybiera losową wersję spośród tych faktycznie ukończonych, których
## do tej pory było najmniej
db.random.condition = function(conditions, task.name = TASK.NAME){
    ct = table(c(db.query.csv(sprintf('select cnd from session where task = %s and stage = "finished"', task.name))$cnd, conditions))
    ct = ct[names(ct) != 'undefined']
    sample(names(ct[ct == min(ct)]), 1)
}

## Tworzy standardową tabelę danych zadania, trzeba podać przykładowe dane jako listę
db.create.data.table = function(data, task.name = TASK.NAME, table.name = 'data'){
    q = ''
    for(i in 1:length(data)){
        if(is.character(data[[i]]) | is.factor(data[[i]])){
            type = 'VARCHAR(50)'
        }else if(is.double(data[[i]])){
            type = 'DOUBLE'
        }else if(is.integer(data[[i]])){
            type = 'INT'
        }
        q = paste(q, names(data)[i], type, ifelse(i == length(data), '', ','))
    }
    q = sprintf('create table if not exists %s_%s (timestamp timestamp, %s);', task.name, table.name, q)
    db.query(q)
}

db.insert.data = function(data, name = TASK.NAME, table.name = "data"){
    ## data$session = session.id
    db.query(db.insert.query(data, sprintf("%s_%s", name, table.name)), quit.after = F)
}

######################################################################
### Ściąganie procedury

download.run.task = function(name = TASK.NAME){
    download.task(name)
    task.log(sprintf("Loading main task script for task \"%s\"", name))
    setwd(sprintf("/taskdata/%s", name))
    source(sprintf("%s.R", name))
}

download.task = function(name = TASK.NAME){
    if(name != ""){
        task.log(paste("Downloading project", name))
        system(sprintf('cd /taskdata && rm -fR %s', name))
        if(system(sprintf('cd /taskdata && git clone https://github.com/boryspaulewicz/%s', name)) != 0){
            gui.error.msg("Nie udało się ściągnąć i rozpakować zadania")
        }
    }
}

######################################################################
### GUI

gui.error.msg = function(txt, w = NULL, quit.after = T){
    task.log(paste("Error:", txt))
    try(WINDOW$close(), T)
    md = gtkMessageDialog(w, 'destroy-with-parent', 'error', 'ok', txt)
    md$run()
    md$destroy()
    ## stop()
    if(quit.after)quit("no")
}

## gui.admin.login = function(){
##     ADMIN.ID <<- 0
##     w = gtkWindow(show = F)
##     w$setPosition('center-always')
##     w$title = "Logowanie"
##     l1 = gtkLabel("Login")
##     login = gtkEntry()
##     l2 = gtkLabel("Hasło")
##     passwd = gtkEntry()
##     passwd$visibility = F
##     btn = gtkButton("Ok")
##     w$add((hb = gtkHBox()))
##     hb$packStart((vb = gtkVBox()), T, F, 10)
##     for(widget in c(l1, login, l2, passwd, btn))vb$packStart(widget, F, F, 10)
##     gSignalConnect(btn, 'clicked', function(btn){
##         admin.row = db.query.csv(sprintf('select * from admin where name = "%s";', login$text))
##         if(login$text %in% admin.row$name){
##             if(admin.row$passwd == db.query.csv(sprintf('select password("%s");', passwd$text))[,1]){
##                 ADMIN.ID <<- admin.row$id
##                 task.log(paste("Succesful admin login for", login$text))
##                 w$destroy()
##                 gtkMainQuit()
##             }else{
##                 gui.error.msg("Błędne hasło", quit.after = F)
##             }
##         }else{
##             gui.error.msg('Nie ma takiego loginu', quit.after = F)
##         }
##     })
##     gSignalConnect(w, 'delete-event', function(w, ...)gtkMainQuit())
##     w$show()
##     gtkMain()
## }

gui.run.task = function(){
    w = gtkWindow(show = F)
    w$setPosition('center-always')
    w$title = "Uruchomienie zadania"
    l3 = gtkLabel("Zadanie")
    task.name = gtkEntry()
    btn = gtkButton("Ok")
    w$add((hb = gtkHBox()))
    hb$packStart((vb = gtkVBox()), T, F, 10)
    for(widget in c(l3, task.name, btn))vb$packStart(widget, F, F, 10)
    gSignalConnect(btn, 'clicked', function(btn){
        TASK.NAME <<- task.name$text
        w$destroy()
        gtkMainQuit()
    })
    gSignalConnect(w, 'delete-event', function(w, ...)gtkMainQuit())
    w$show()
    gtkMain()
    if(TASK.NAME != ""){
        download.run.task(TASK.NAME)
    }else{
        gui.error.msg("Nie można uruchomić zadania - pusta nazwa")
    }
}

## Tworzy okno z listą nazw zadań z lewej strony i polem opisu
## wybranego zadania z prawej.
gui.select.task = function(){
    TASK.ID <<- 0
    w = gtkWindow(show = F)
    w$setTitle("Wybór zadania")
    w$setPosition('center-always')
    w$add((hb = gtkHBox()))
    ## Lewa kolumna okna
    hb$packStart((vb = gtkVBox()))
    projects = db.query.csv('select * from project')
    vb$packStart(gtkLabel("Nazwa zadania"), F, F)
    vb$packStart((cb = gtkComboBoxNewText()), F, F, 10)
    for(n in projects$name){
        cb$appendText(n)
    }
    vb$packStart(gtkLabel(""), T, T) ## żeby zepchnąć przycisk na dół
    vb$packStart((b = gtkButton("Uruchom")), F, F)
    ## Zamiast zmiennej globalnej
    attributes(b)$run = F
    ## Prawa kolumna - opis
    hb$packStart((f = gtkFrame("Opis")), F, F, 10)
    f$add((tv = gtkTextView()))
    tv$setWrapMode('word')
    tv$setEditable(F)
    tv$setBuffer((tb = gtkTextBuffer()))
    f$setSizeRequest(300, 300)
    ## Obsługa zdarzenia wyboru itemu
    gSignalConnect(cb, 'changed', function(w, ...){
        tb$setText(projects$desc[cb$active + 1])
    })
    ## Obsługa przycisku Uruchom
    gSignalConnect(b, 'clicked', function(widget){
        if((cb$active >= 0) & (!attributes(b)$run)){
            TASK.ID <<- cb$active + 1
            TASK.NAME <<- projects$name[cb$active + 1]
            attributes(b)$run = T
        }
    })
    gSignalConnect(w, 'delete-event', function(w, ...){
        TASK.ID <<- -10
        gtkMainQuit()
    })
    ## Pętla czekania na przycisk
    w$show()
    while(T){
        if(TASK.ID == -10){
            break
        }else{
            gtkMainIteration()
            if(attributes(b)$run){
                b$setLabel('Ściągam')
                gtkMainIteration() ## żeby pokazać zmianę etykiety przycisku
                name = projects$name[cb$active + 1]
                download.task(name)
                w$destroy()
                ## source(sprintf('/taskdata/%s/%s.R', name, name), chdir = T)
            }
        }
    }
}

gui.show.instruction = function(txt, buttons = 'Dalej'){
    CHOSEN.BUTTON <<- ''
    w = gtkWindow(show = F)
    w$setTitle("Instrukcja")
    w$setPosition('center-always')
    w$add((vb = gtkVBox()))
    vb$packStart((tv = gtkTextView()), T, T, 10)
    tv$setWrapMode('word')
    tv$setEditable(F)
    tv$setSizeRequest(600, 400)
    tv$setBuffer((tb = gtkTextBuffer()))
    tb$setText(txt)
    vb$packStart((hb = gtkHBox()), F, F, 10)
    for(b in buttons){
        hb$packStart((btn = gtkButton(b)), F, F, 10)
        gSignalConnect(btn, 'clicked', function(btn, b){
            CHOSEN.BUTTON <<- b
        }, b)
    }
    w$show()
    while(T){
        gtkMainIteration()
        if(CHOSEN.BUTTON != '')break
        if(class(w)[1] == '<invalid>')break;
    }
    if(class(w)[1] != '<invalid>')w$destroy()
    CHOSEN.BUTTON
}

gui.user.data = function(){
    USER.DATA <<- list()
    w = gtkWindow(show = F)
    w$setPosition('center-always')
    w$title = "Dane osobowe"
    w$add((hb = gtkHBox()))
    hb$packStart((vb = gtkVBox()), F, F, 20)
    vb$packStart(gtkLabel("Identyfikator"), F, F, 10)
    vb$packStart((name = gtkEntry()))
    vb$packStart(gtkLabel("Płeć"))
    vb$packStart((gender = gtkComboBoxNewText()))
    vb$packStart(gtkLabel("Wiek"))
    vb$packStart((age = gtkEntry()))
    vb$packStart((btn = gtkButton("Ok")), F, F, 10)
    for(g in c("K", "M"))gender$appendText(g)
    gSignalConnect(btn, 'clicked', function(btn){
        if(name$text == "admin"){
            USER.DATA <<- list(name = "admin", age = 0, gender = "M")
            w$destroy()
            return(T)
        }
        msg = ''
        if(gender$active < 0)msg = paste(msg, 'Nie wybrano płci\n', sep = '')
        if(is.na(as.numeric(age$text)))msg = paste(msg, 'Błąd w polu wieku\n', sep = '')
        if(length(grep('^[a-z][a-z][0-9][0-9][0-9][0-9]$', name$text)) == 0)msg =
            paste(msg, 'Identyfikator musi się składac z dwóch liter (inicjałów),\n dnia (dwie cyfry) i miesiąca (dwie cyfry) urodzenia\n', sep = '')
        if(msg != ''){
            gui.error.msg(msg, quit.after = F)
        }else{
            USER.DATA <<- list(gender = c("K", "M")[gender$active + 1],
                               age = as.numeric(age$text),
                               name = tolower(name$text))
            w$destroy()
        }
    })
    w$show()
    while(class(w)[1] != '<invalid>'){
        gtkMainIteration()
    }
    USER.DATA
}

## Do wprowadzania danych kwestionariuszowych przez administratora badania
gui.quest = function(items, answers, title = 'Dane kwestionariuszowe', width = .5){
    res = rep(-1, length(items))
    while(any(res == -1)){
        res = gui.quest.win(items = items, answers = answers, width = width, title = title, values = res)
    }
    res
}

## Funkcja, która powinna być wywoływana przez gui.quest - żeby
## zamknięcie okna nie było możliwe do momentu wypełnienia całości
gui.quest.win = function(items, answers, title = 'Dane kwestionariuszowe', width = .5, values = NULL){
    w = gtkWindow()
    w$setPosition('center-always')
    w$title = title
    w$deletable = F
    w$add((b0 = gtkHBox()))
    b0$packStart((b1 = gtkVBox()), T, T, 10)
    b1$packStart((scroll = gtkScrolledWindow()), T, T, 10)
    scroll$addWithViewport((vb = gtkVBox()))
    gui.items = NULL
    gui.values = NULL
    label.width = 0
    for(i in 1:length(items)){
        cb = gtkComboBoxNewText()
        for(a in answers)cb$appendText(a)
        if(!is.null(values))cb$active = values[[i]]
        gui.items = append(gui.items, cb)
        gui.values = append(gui.values, -1)
        vb$packStart((hb = gtkHBox()), F, F)
        hb$packStart((il = gtkLabel(items[[i]])), T, T)
        il$justify = 'left'
        il$setAlignment(0, .5)
        label.width = max(label.width, gtkRequisitionGetWidth(il$sizeRequest()$requisition))
        hb$packStart(cb, F, F)
    }
    b1$packStart((bb = gtkHBox()), F, F, 10)
    bb$packStart((btn = gtkButton("Zatwierdź")), F, F, 10)
    w$setSizeRequest(min(label.width + gtkRequisitionGetWidth(cb$sizeRequest()$requisition), WINDOW$get.size()[1] * width),
                     min(gtkRequisitionGetHeight(vb$sizeRequest()$requisition) * 1.2, WINDOW$get.size()[2] * .9))
    gSignalConnect(w, 'delete-event', function(w, ...){
        for(i in 1:length(gui.items))gui.values[[i]] <<- gui.items[[i]]$active
        gtkMainQuit()
    })
    gSignalConnect(btn, 'clicked', function(btn){
        done = T
        for(i in 1:length(gui.items)){
            gui.values[[i]] <<- gui.items[[i]]$active
            if(gui.values[[i]] == -1)done = F
        }
        if(done){
            w$destroy()
            gtkMainQuit()
        }else{
            gui.error.msg("Nie udzielono wszystkich odpowiedzi", quit.after = F)
        }
    })
    w$show()
    gtkMain()
    gui.values
}

## gui.quest(items = paste('Pytanie numer', 1:20), answers = 1:4)

######################################################################
### Media

## Jeżeli nie ma głównego okna, albo jest zamknięte, to tworzymy
if((!exists('WINDOW')) || (class(WINDOW) == 'function') ){
    WINDOW = window.fullscreen("Tytuł")
}else if(!WINDOW$is.open()){
    WINDOW = window.fullscreen("Tytuł")
}

## Globalne zmienne mediów

WINDOW$set.visible(F)
WINDOW$set.mouse.cursor.visible(F)
WINDOW$set.vertical.sync.enabled(T)
WIDTH = WINDOW$get.size()[1]
HEIGHT = WINDOW$get.size()[2]
EVENT = new(Event)
CLOCK = new(Clock)
FONT = new(Font)
if(!FONT$load.from.file('/usr/share/fonts/truetype/lato/Lato-Regular.ttf'))stop("Could not load the font")
TXT = new(Text)
TXT$set.font(FONT)
KEY.PRESSED = KEY.RELEASED = rep(0, Key.KeyCount + 1)
BUTTON.PRESSED = BUTTON.RELEASED = rep(0, Button.ButtonCount + 1)

KEYS = CORRECT.KEY = BUTTONS = ACC = RT = NULL

## Małe funkcje pomocnicze

center.win = function(x)center(x, WINDOW)

## Standardowe przetwarzanie wejść
process.inputs = function(){
    while(WINDOW$poll.event(EVENT)){
        if(EVENT$type == EventType.Closed){
            WINDOW$close()
        }else if(EVENT$type == EventType.KeyPressed){
            tm = CLOCK$time
            code = EVENT$key$code
            KEY.PRESSED[code + 1] <<- tm
            if((code %in% KEYS)){
                RT <<- tm
                ACC <<- as.numeric(CORRECT.KEY == code)
            }
        }else if(EVENT$type == EventType.KeyReleased){
            KEY.RELEASED[EVENT$key$code + 1] <<- CLOCK$time
        }else if(EVENT$type == EventType.MouseButtonPressed){
            BUTTON.PRESSED[EVENT$mouse.button$button + 1] <<- CLOCK$time
        }else if(EVENT$type == EventType.MouseButtonReleased){
            BUTTON.RELEASED[EVENT$mouse.button$button + 1] <<- CLOCK$time
        }
    }
}

## dst to Image
draw.sin = function(dst, f = 20, angle = 45, contrast = 1.0, sigma = 0.25, mask = F){
    width = dst$size[1] ## WINDOW$get.size()[1]
    height = dst$size[2] ## WINDOW$get.size()[2]
    sigma = sigma * WINDOW$get.size()[1] ## width
    angle = angle * pi / 180.0
    cos_angle = cos(angle)
    sin_angle = sin(angle)
    sigma2dbl = 2 * (sigma^2)
    x_middle = width / 2
    y_middle = height / 2
    two.pi.by.height = 2 * pi / WINDOW$get.size()[2] ## height
    ## Parametry maski
    altered.col = 1
    col.size = 1
    sq.w = sq.h = .5 / f * WINDOW$get.size()[2]
    ## Jasność określona przez odległość (kwadrat dla oszczędzenia liczenia) od środka ekranu
    for(y in 0:(height-1)){
        ## if(show_progress)progress(y / height)
        for(x in 0:(width-1)){
            ## distance2 = (power2(float(x - x_middle)) + power2(float(y - y_middle))) / sigma2
            visibility = exp(-(((x - x_middle)^2 + (y - y_middle)^2) / sigma2dbl))
            ## Dla distance = 0 mamy exp(-distance2/2.0) == 1
            scaling = 0.5 * contrast * visibility
            if(scaling > 0){
                if(mask){
                    i = as.numeric((x %% (sq.w * 2)) > sq.w)
                    if((y %% (sq.w * 2)) > sq.w)i = 1 - i
                    if((i == altered.col) && (col.size < 1.0)){
                        ## Białe rysujemy tylko wtedy, gdy x i y znajdują
                        ## się w obrębie pod-kwadrata określonego przez
                        ## white_size
                        xsq = x %% sq.w
                        ysq = y %% sq.w
                        if((xsq > (col.size * sq.w)) || (xsq < ((1.0 - col.size) * sq.w)) ||
                           (ysq > (col.size * sq.w)) || (ysq < ((1.0 - col.size) * sq.w)))i = 1 - altered.col
                    }
                    res = (i - 0.5) * visibility + 0.5
                    dst$set.pixel(x, y, c(res, res, res))
                }else{
                    ## Tylko jeżeli w tym punkcie widoczność jest niezerowa, to rysujemy
                    ##
                    ## Obracamy układ współrzędnych
                    new_y = y * cos_angle + x * sin_angle
                    v = 0.5 + sin(new_y * two.pi.by.height * f) * scaling
                    dst$set.pixel(x, y, c(v, v, v))
                }
            }else{
                dst$set.pixel(x, y, c(.5, .5, .5))
            }
        }
    }
}

## Wymiary skali
SCALE.WIDTH = .8
SCALE.POSITION = .6
SCALE.HEIGHT = .05
draw.scale = function(labels = c('LOW', 'AVERAGE', 'HIGH'), position = SCALE.POSITION, width = SCALE.WIDTH, height = SCALE.HEIGHT,
                      background.color = c(0, 0, 0), gradient = F, draw.bar = T, highlight.label = F, highlight.box = T,
                      label.scale = 1){
    ## Obiekt do rysowania etykiet
    label = new(Text)
    label$set.font(FONT)
    label$set.scale(c(label.scale, label.scale))
    ## Kreseczka dla wersji ciągłej
    bar = new(RectangleShape, c(WINDOW$get.size() * c(width * .01, height)))
    bar$set.fill.color(c(0, 0, 1, .5))
    bar$set.origin(bar$get.local.bounds()[3:4] * .5)
    ## Pojedyncze pudełko - wymiary
    if(length(labels) == 2){
        rect.dims = WINDOW$get.size() * c(width, height)
    }else{
        rect.dims = WINDOW$get.size() * c(width / length(labels), height)
    }
    rect = new(RectangleShape, rect.dims)
    if(length(labels) == 2){
        rect$set.origin(rect.dims / 2)
    }else{
        rect$set.origin(c(0, rect.dims[2] / 2)) ## początek pudełka z lewej po środku
    }
    rect$set.fill.color(background.color)
    rect$set.outline.color(c(1, 1, 1))
    rect$set.outline.thickness(1)
    rect.bounds = rect$get.local.bounds()
    ## Położenie lewej krawędzi pudełek na ekranie
    scale.origin = WINDOW$get.size() * c((1 - width) / 2, position)
    rect$set.position(scale.origin)
    ## Ustalamy, która opcja jest wskazywana
    mp.raw = mouse.get.position()
    mp = (mp.raw[1] - scale.origin[1]) / (width * WINDOW$get.size()[1])
    chosen = length(labels) + 1
    if((mp >= 0) && (mp <= 1)){
        chosen = ceiling(mp / (1 / length(labels)))
    }else{
        chosen = length(labels) + 1
    }
       ## (mp.raw[2] >= rect.bounds['top']) &&
       ## (mp.raw[2] <= sum(rect.bounds[c(2, 4)])))
    pointed = max(min(mp, 1), 0)
    bar$set.position(scale.origin + c(WINDOW$get.size()[1] * width * pointed, 0))
    if(length(labels) == 2){
        ## Ewentualny pasek bez podziału na pudełka
        rect$set.position(WINDOW$get.size() * c(.5, position))
        WINDOW$draw(rect)
        for(i in 1:2){
            label$set.string(labels[i])
            ## Gdy z lewej, to zaczepienie etykiety z lewej (0, top +
            ## .5 * height), gdy z prawej, to zaczepienie z prawej
            ## (width, ...)
            bounds = label$get.local.bounds()
            ## Dla pierwszej etykiety origin jest z lewej, dla drugiej z prawej
            label$set.origin(c(c(0, bounds['width'])[i], bounds[c('top', 'height')] %*% c(1, .5)))
            label$set.position(scale.origin + c(c(0, width * WINDOW$get.size()[1])[i],
                                                bounds['height'] + WINDOW$get.size()[2] * height))
            label$set.color(c(1, 1, 1))
            WINDOW$draw(label)
        }
    }else{
        ## Rysujemy pudełka niepodświetlone
        for(i in c(1:length(labels))[-chosen]){
            if(gradient)rect$set.fill.color(rep(i / (length(labels) + 1), 3))
            rect$set.position(scale.origin + c(rect.dims[1] * (i-1), 0))
            WINDOW$draw(rect)
        }
        ## Rysujemy pudełko podświetlone
        if(chosen < (length(labels) + 1)){
            if(gradient)rect$set.fill.color(rep(chosen / (length(labels) + 1), 3))
            rect$set.position(scale.origin + c(rect.dims[1] * (chosen-1), 0))
            rect$set.outline.thickness(3)
            WINDOW$draw(rect)
            rect$set.outline.thickness(1)
        }
        ## Rysujemy etykiety
        for(i in 1:length(labels)){
            label$set.string(labels[i])
            bounds = label$get.local.bounds()
            label$set.origin(c(bounds['width'] / 2, bounds['top'] + bounds['height'] / 2))
            label$set.position(c(scale.origin[1] + (i - .5) * rect.dims[1], scale.origin[2]))
            if((i == chosen) && highlight.label){
                label$set.color(c(0, 1, 0))
            }else{
                label$set.color(c(1, 1, 1))
            }
            WINDOW$draw(label)
        }
    }
    if(draw.bar)WINDOW$draw(bar)
    ## Zwracamy wskazywany punkt
    c(pointed, chosen)
}

######################################################################
### Wykonanie procedury

## Wersja losowana na podstawie nazw plików konfiguracyjnych, jeżeli
## są jakieś, zwraca nazwę warunku
source.random.condition = function(){
    cnds = dir('./condition/', '\\.R$')
    if(length(cnds) == 0){
        cnd = 'undefined'
    }else{
        cnd = db.random.condition(cnds)
        source(paste('./condition/', cnd, sep = ''))
    }
    cnd
}

## trial.code jest wykonywana na losowanych warunkach, wartości
## czynników definiujących warunki są jej przekazywane jako argumenty
run.trials = function(trial.code, cnd, b = 1, n = 1,
                      data.table = "data", max.time = NULL, nof.trials = NULL, condition = NULL, record.session = F){
    if('trial' %in% names(cnd))stop('trial is not a valid factor name')
    create.table = !(paste(TASK.NAME, data.table, sep = '_') %in% db.query.csv('show tables')[,1])
    if(is.null(nof.trials)){
        nof.trials = nrow(cnd) * b * n
    }else{
        n = 1
        b = ceiling(nof.trials / nrow(cnd))
    }
    scenario = NULL
    for(i in 1:n)scenario = c(scenario, sample(rep(1:nrow(cnd), b)))
    WINDOW$set.visible(T)
    TASK.START <<- CLOCK$time
    task.log(sprintf("Starting task %s by user %s", TASK.NAME, USER.DATA$name))
    for(trial in 1:nof.trials){
        args = as.list(cnd[scenario[trial],])
        names(args) = names(cnd)
        args[['trial']] = trial
        gc()
        data = do.call(trial.code, args)
        if(!is.null(data.table)){
            all.data = append(USER.DATA, append(args, data))
            if(trial == 1){
                if(record.session){
                    if(create.table){
                        task.log(paste("Creating table for task", TASK.NAME))
                        db.create.data.table(all.data, table.name = data.table)
                    }
                    ## Zapisujemy fakt, że rozpoczęto sesję zadania
                    db.query(sprintf('insert into session (task, id, cnd, stage) values ("%s", "%s", "%s", "started")',
                                     TASK.NAME, USER.DATA$name, condition))
                }
            }
            if(record.session)db.insert.data(all.data, table.name = data.table)
        }
        if(is.null(data) || (!is.null(max.time) && (CLOCK$time - TASK.START) > max.time))break
    }
    task.log(sprintf("Completed task %s by user %s", TASK.NAME, USER.DATA$name))
    if(record.session)db.query(sprintf('insert into session (task, id, stage) values ("%s", "%s", "%s", "finished")',
                                       TASK.NAME, USER.DATA$name, condition))
    WINDOW$set.visible(F)
}

######################################################################
### Inicjalizacja

DB.IP <<- db.ip()
if(!interactive()){
    ## gui.admin.login()
    ## if(ADMIN.ID > 0){
    ## ADMIN.ID <<- 1
    gui.run.task()
    ## }else{ quit("no") }
}

## res = gui.quest(paste('Pytanie', 1:20), 1:4)
