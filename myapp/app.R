
# packages ----------------------------------------------------------------

library(shiny)
library(jpeg)
library(RCurl)
library(data.table)
library(magrittr)
library(dplyr)
library(leaflet)
library(jsonlite)

# 0) SUPABASE CONFIG ------------------------------------------------------
# Remplace ces deux valeurs par celles de TON projet Supabase
# (Project Settings > API). La "anon public key" peut etre exposee
# publiquement TANT QUE la Row Level Security (RLS) est activee.
# N'utilise JAMAIS la cle "service_role" ici.

supabase_url <- "https://tufsppsscnymsjcfextw.supabase.co"
supabase_key <- "sb_publishable_kQpUXGEm9tqY2xwbxNphyw_wRar_KMV"

# 1) local paths ----------------------------------------------------------

dt <- fread("data/allsp.csv")

# Initialize score
score <- 0

df <- dt
# French names for the taxa

taxa <- c(
  "Zamenis longissimus",
  "Vipera aspis",
  "Vipera berus",
  "Natrix natrix",
  "Natrix helvetica",
  "Coronella austriaca",
  "Hierophis viridiflavus",
  "Natrix maura",
  "Natrix tessellata"
)

taxa_french <- c(
  "Zamenis longissimus (Couleuvre d'Esculape)",
  "Vipera aspis (Vipère aspic)",
  "Vipera berus (Vipère péliade)",
  "Natrix natrix (Couleuvre à collier)",
  "Natrix helvetica (Couleuvre à collier helvétique)",
  "Coronella austriaca (Coronelle lisse)",
  "Hierophis viridiflavus (Couleuvre verte et jaune)",
  "Natrix maura (Couleuvre vipérine)",
  "Natrix tessellata (Couleuvre tessellée)"
)

# Helpers -----------------------------------------------------------------

format_time <- function(secs) {
  if (is.null(secs) || is.na(secs)) return("00:00")
  secs <- round(as.numeric(secs))
  m <- secs %/% 60
  s <- secs %% 60
  sprintf("%02d:%02d", m, s)
}

empty_lb <- function() {
  data.frame(
    name = character(0),
    score = integer(0),
    total = integer(0),
    time = numeric(0),
    stringsAsFactors = FALSE
  )
}

# JavaScript / CSS for the zoom lightbox + Supabase leaderboard -----------

zoom_js <- "
(function(){
  // ---- Supabase config (injected from R) ----
  const SB_URL = '__SB_URL__';
  const SB_KEY = '__SB_KEY__';
  const SB_HEADERS = {
    'apikey': SB_KEY,
    'Authorization': 'Bearer ' + SB_KEY,
    'Content-Type': 'application/json'
  };

  // ---- Image zoom lightbox ----
  let overlay, imgEl, scale=1, tx=0, ty=0, dragging=false, sx=0, sy=0;
  let pinchDist=0, pinchScale=1;

  function apply(){
    if(imgEl) imgEl.style.transform='translate('+tx+'px,'+ty+'px) scale('+scale+')';
  }
  function reset(){ scale=1; tx=0; ty=0; apply(); }

  function ensureOverlay(){
    if(overlay) return;
    overlay=document.createElement('div');
    overlay.id='zoom-overlay';
    overlay.innerHTML=
      '<div id=\"zoom-close\">&#10005;</div>'+
      '<img id=\"zoom-img\" draggable=\"false\"/>'+
      '<div id=\"zoom-controls\">'+
        '<button type=\"button\" data-z=\"out\">&minus;</button>'+
        '<button type=\"button\" data-z=\"reset\">&#8634;</button>'+
        '<button type=\"button\" data-z=\"in\">+</button>'+
      '</div>'+
      '<div id=\"zoom-hint\">Molette / pincement : zoom &middot; glisser : déplacer &middot; Échap : fermer</div>';
    document.body.appendChild(overlay);
    imgEl=document.getElementById('zoom-img');

    overlay.addEventListener('click', function(e){
      if(e.target===overlay || e.target.id==='zoom-close') close();
    });
    overlay.querySelector('#zoom-controls').addEventListener('click', function(e){
      var b=e.target.closest('button'); if(!b) return;
      var z=b.getAttribute('data-z');
      if(z==='in') scale=Math.min(scale*1.3, 12);
      if(z==='out') scale=Math.max(scale/1.3, 1);
      if(z==='reset') reset();
      if(scale===1){ tx=0; ty=0; }
      apply();
    });
    overlay.addEventListener('wheel', function(e){
      e.preventDefault();
      var delta=e.deltaY<0?1.15:1/1.15;
      scale=Math.min(Math.max(scale*delta,1),12);
      if(scale===1){ tx=0; ty=0; }
      apply();
    }, {passive:false});

    imgEl.addEventListener('mousedown', function(e){
      dragging=true; sx=e.clientX-tx; sy=e.clientY-ty; e.preventDefault();
    });
    window.addEventListener('mousemove', function(e){
      if(!dragging) return; tx=e.clientX-sx; ty=e.clientY-sy; apply();
    });
    window.addEventListener('mouseup', function(){ dragging=false; });

    imgEl.addEventListener('touchstart', function(e){
      if(e.touches.length===1){
        dragging=true; sx=e.touches[0].clientX-tx; sy=e.touches[0].clientY-ty;
      } else if(e.touches.length===2){
        dragging=false;
        var dx=e.touches[0].clientX-e.touches[1].clientX;
        var dy=e.touches[0].clientY-e.touches[1].clientY;
        pinchDist=Math.hypot(dx,dy); pinchScale=scale;
      }
    }, {passive:false});
    imgEl.addEventListener('touchmove', function(e){
      e.preventDefault();
      if(e.touches.length===1 && dragging){
        tx=e.touches[0].clientX-sx; ty=e.touches[0].clientY-sy; apply();
      } else if(e.touches.length===2){
        var dx=e.touches[0].clientX-e.touches[1].clientX;
        var dy=e.touches[0].clientY-e.touches[1].clientY;
        var d=Math.hypot(dx,dy);
        if(pinchDist>0){ scale=Math.min(Math.max(pinchScale*(d/pinchDist),1),12); apply(); }
      }
    }, {passive:false});
    imgEl.addEventListener('touchend', function(e){
      if(e.touches.length===0) dragging=false;
    });

    document.addEventListener('keydown', function(e){ if(e.key==='Escape') close(); });
  }

  function open(src){ ensureOverlay(); reset(); imgEl.src=src; overlay.style.display='flex'; }
  function close(){ if(overlay) overlay.style.display='none'; }

  document.addEventListener('click', function(e){
    var t=e.target;
    if(t && t.classList && t.classList.contains('zoomable')){ open(t.src); }
  });

  // ---- Shared leaderboard via Supabase REST ----
  function loadLeaderboard(){
    fetch(SB_URL + '/rest/v1/leaderboard?select=name,score,total,time&order=score.desc,time.asc&limit=100',
          { headers: SB_HEADERS })
      .then(function(r){
        if(!r.ok){
          return r.text().then(function(t){
            console.error('Supabase LECTURE échouée — statut', r.status, t);
            return [];
          });
        }
        return r.json();
      })
      .then(function(data){
        console.log('Classement chargé:', data);
        Shiny.setInputValue('lb_loaded', JSON.stringify(data), {priority:'event'});
      })
      .catch(function(e){
        console.error('Supabase LECTURE erreur réseau:', e);
        Shiny.setInputValue('lb_loaded', '[]', {priority:'event'});
      });
  }
  $(document).on('shiny:connected', loadLeaderboard);

  // R asks us to add a score, then we reload the table
  Shiny.addCustomMessageHandler('add_score', function(entry){
    var body;
    try {
      // entry may arrive as a JSON string OR as an already-parsed object
      body = (typeof entry === 'string') ? JSON.parse(entry) : entry;
    } catch(e){ console.error('Entrée illisible', e, entry); return; }
    console.log('Envoi du score à Supabase:', body);
    fetch(SB_URL + '/rest/v1/leaderboard', {
      method: 'POST',
      headers: Object.assign({}, SB_HEADERS, {'Prefer':'return=representation'}),
      body: JSON.stringify(body)
    })
    .then(function(r){
      return r.text().then(function(t){
        if(!r.ok){
          console.error('Supabase INSERT échoué — statut', r.status, t);
          alert('Erreur Supabase (' + r.status + ') :\\n' + t);
        } else {
          console.log('Insert OK:', t);
        }
        loadLeaderboard();
      });
    })
    .catch(function(e){
      console.error('Supabase INSERT erreur réseau:', e);
      alert('Erreur réseau Supabase : ' + e);
      loadLeaderboard();
    });
  });
})();
"

# inject Supabase credentials into the JS
zoom_js <- gsub("__SB_URL__", supabase_url, zoom_js, fixed = TRUE)
zoom_js <- gsub("__SB_KEY__", supabase_key, zoom_js, fixed = TRUE)

zoom_css <- "
.zoomable{ cursor: zoom-in; }
#zoom-overlay{
  display:none; position:fixed; inset:0; background:rgba(0,0,0,.92);
  z-index:99999; align-items:center; justify-content:center; overflow:hidden;
}
#zoom-img{
  max-width:92vw; max-height:88vh; transform-origin:center center;
  will-change:transform; user-select:none; -webkit-user-drag:none; touch-action:none;
}
#zoom-close{
  position:fixed; top:12px; right:20px; color:#fff; font-size:30px;
  cursor:pointer; z-index:100000; line-height:1;
}
#zoom-hint{
  position:fixed; bottom:14px; left:0; right:0; text-align:center;
  color:#ddd; font-size:13px; z-index:100000; padding:0 10px;
}
#zoom-controls{
  position:fixed; bottom:46px; left:0; right:0; text-align:center; z-index:100000;
}
#zoom-controls button{
  font-size:22px; width:46px; height:46px; margin:0 6px; border:none;
  border-radius:10px; background:#fff; color:#222; cursor:pointer;
}
.lb-table{ width:100%; border-collapse:collapse; margin-top:8px; }
.lb-table th, .lb-table td{ padding:6px 10px; text-align:left; border-bottom:1px solid #e3e3e3; }
.lb-table th{ background:#f5f5f5; }
.lb-table td:first-child{ width:42px; text-align:center; }
.timer-box{ font-size:18px; font-weight:600; margin:6px 0; }
"

# Define UI ----------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML(zoom_css)),
    tags$script(HTML(zoom_js))
  ),
  titlePanel("Herpeto Quiz"),
  sidebarLayout(
    sidebarPanel(
      textInput("player_name", "Ton nom (obligatoire) :", placeholder = "ex: Tartempion"),
      p("Le quiz porte sur toutes les espèces de serpents de Suisse."),
      actionButton("start_game", "Start Game"),
      hr(),
      h4("Instructions"),
      p("Sélectionne l'espèce correcte dans la liste ci-après."),
      selectInput("answer", "choisis l'espèce:", choices = NULL),
      actionButton("submit", "Submit"),
      textOutput("feedback"),
      hr(),
      div(class = "timer-box", textOutput("timer")),
      textOutput("score"),
      br(),
      actionButton("restart", "Restart Game")
    ),
    mainPanel(
      uiOutput("image"),
      leafletOutput("map"),
      hr(),
      h4("🏆 Classement"),
      uiOutput("leaderboard_ui")
    )
  )
)

# Define server logic ------------------------------------------------------
server <- function(input, output, session) {

  # Initialize reactive values
  current_index <- reactiveVal(1)
  score <- reactiveVal(0)
  game_over <- reactiveVal(FALSE)
  df <- reactiveVal(data.frame())  # Empty reactive dataframe initially

  player      <- reactiveVal("")
  start_time  <- reactiveVal(NULL)
  final_time  <- reactiveVal(NULL)
  leaderboard <- reactiveVal(empty_lb())

  # Load leaderboard from Supabase (sent by JS)
  observeEvent(input$lb_loaded, {
    parsed <- tryCatch(jsonlite::fromJSON(input$lb_loaded), error = function(e) NULL)
    if (is.data.frame(parsed) && nrow(parsed) > 0) {
      for (col in c("name", "score", "total", "time")) {
        if (is.null(parsed[[col]])) parsed[[col]] <- NA
      }
      leaderboard(parsed[, c("name", "score", "total", "time")])
    } else {
      leaderboard(empty_lb())
    }
  }, ignoreInit = TRUE)

  # Start the game (quiz on ALL species)
  observeEvent(input$start_game, {

    nm <- trimws(if (is.null(input$player_name)) "" else input$player_name)
    if (nm == "") {
      showNotification("Entre ton nom avant de commencer !", type = "error")
      return()
    }
    player(nm)

    # all species
    filtered_dt <- dt[dt$scientific_name %in% taxa, ]

    # Generate a new random sample
    random_number <- sample(0:90000, 1)
    set.seed(random_number)
    new_sample <- filtered_dt %>%
      group_by(scientific_name) %>%
      slice_sample(n = ceiling(100 / length(taxa))) %>%
      ungroup() %>%
      slice_sample(n = 100) %>%
      slice_sample(n = 15)  # 15 questions

    df(new_sample)  # Update the reactive dataframe

    # Reset game state
    current_index(1)
    score(0)
    game_over(FALSE)
    final_time(NULL)
    start_time(Sys.time())          # start the chronometer
    output$feedback <- renderText("")

    # Answer dropdown = all species
    updateSelectInput(session, "answer", choices = taxa_french)
  })

  # Chronometer display
  output$timer <- renderText({
    if (!is.null(final_time())) {
      paste("⏱️ Temps :", format_time(final_time()))
    } else if (!is.null(start_time()) && !game_over()) {
      invalidateLater(1000, session)
      elapsed <- as.numeric(difftime(Sys.time(), start_time(), units = "secs"))
      paste("⏱️ Temps :", format_time(elapsed))
    } else {
      "⏱️ Temps : 00:00"
    }
  })

  # Display the current image
  output$image <- renderUI({
    if (!game_over() && nrow(df()) > 0) {
      i <- current_index()
      image_url <- df()[i, ]$image_url

      tags$div(
        style = "text-align: center;",
        tags$img(
          src = image_url,
          class = "zoomable",
          style = "max-width: 90%; max-height: 600px; border: 2px solid #ccc;"
        )
      )
    } else {
      final_score <- score()
      message <- ""
      image_url <- ""

      if (final_score <= 5) {
        message <- "bravo, tu as le niveau d'un orvet."
        image_url <- "https://inaturalist-open-data.s3.amazonaws.com/photos/280317402/medium.jpg"
      } else if (final_score <= 10) {
        message <- "tu es l'équivalent de ce crapaud "
        image_url <- "https://inaturalist-open-data.s3.amazonaws.com/photos/88307277/medium.jpeg"
      } else if (final_score <= 14) {
        message <- "t'es pas plus con que t'en as l'air!"
        image_url <- "https://inaturalist-open-data.s3.amazonaws.com/photos/448385907/large.jpg"
      } else {
        message <- "tu es l'équivalent d'un anaconda dans ce jeu, c'est a dire une soupière tropicale"
        image_url <- "https://inaturalist-open-data.s3.amazonaws.com/photos/145377510/original.jpg"
      }

      tags$div(
        style = "text-align: center; font-size: 20px; color: red;",
        tags$h3("Fini!"),
        tags$p(paste(
          "ton score est de :", score(), "/", nrow(df()), "points en",
          format_time(final_time()), ".", message
        )),
        tags$img(
          src = image_url,
          class = "zoomable",
          style = "max-width: 90%; max-height: 600px; border: 2px solid #ccc;"
        )
      )
    }
  })

  # Render the map
  output$map <- renderLeaflet({
    i <- current_index()
    if (!game_over() && nrow(df()) > 0 && i <= nrow(df())) {
      lat <- df()[i, ]$latitude
      lon <- df()[i, ]$longitude

      leaflet() %>%
        addTiles() %>%
        setView(lng = lon, lat = lat, zoom = 10) %>%
        addMarkers(lng = lon, lat = lat, popup = paste("Observation:", df()[i, ]$scientific_name))
    } else {
      leaflet() %>%
        addTiles()
    }
  })

  # Check the user's input
  observeEvent(input$submit, {
    i <- current_index()
    if (!game_over() && nrow(df()) > 0 && i <= nrow(df())) {
      correct_species <- df()[i, ]$scientific_name
      correct_species_french <- taxa_french[which(taxa == correct_species)]
      if (input$answer == correct_species_french) {
        score(score() + 1)
        output$feedback <- renderText("Bien joué tartempion!")
      } else {
        output$feedback <- renderText(paste("...m'enfin? l'espèce était:", correct_species_french))
      }
      current_index(i + 1)

      if (i == nrow(df())) {
        game_over(TRUE)

        # freeze the chronometer
        ft <- as.numeric(difftime(Sys.time(), start_time(), units = "secs"))
        final_time(ft)

        # send the run to Supabase (JS handles the POST + reload)
        new_entry <- data.frame(
          name  = player(),
          score = as.integer(score()),
          total = as.integer(nrow(df())),
          time  = as.integer(round(ft)),
          stringsAsFactors = FALSE
        )
        # as.character() strips the "json" class so Shiny sends plain TEXT
        # (otherwise Shiny inlines it as an object and JSON.parse fails JS-side)
        session$sendCustomMessage(
          "add_score",
          as.character(jsonlite::toJSON(new_entry, dataframe = "rows", auto_unbox = TRUE))
        )
      }
    }
  })

  # Display the score
  output$score <- renderText({
    paste("Score:", score(), "/", nrow(df()))
  })

  # Leaderboard table
  output$leaderboard_ui <- renderUI({
    lb <- leaderboard()
    if (is.null(lb) || nrow(lb) == 0) {
      return(tags$p("Aucun score enregistré pour l'instant. Lance une partie !"))
    }
    lb <- lb[order(-lb$score, lb$time), ]
    lb <- head(lb, 50)

    rows <- lapply(seq_len(nrow(lb)), function(k) {
      rank_lbl <- if (k == 1) "🥇" else if (k == 2) "🥈" else if (k == 3) "🥉" else paste0(k, ".")
      tags$tr(
        tags$td(rank_lbl),
        tags$td(lb$name[k]),
        tags$td(paste0(lb$score[k], "/", lb$total[k])),
        tags$td(format_time(lb$time[k]))
      )
    })

    tags$table(
      class = "lb-table",
      tags$thead(tags$tr(
        tags$th(""), tags$th("Nom"), tags$th("Score"), tags$th("Temps")
      )),
      tags$tbody(rows)
    )
  })

  # Restart the game
  observeEvent(input$restart, {
    df(data.frame())  # Reset the dataframe
    current_index(1)
    score(0)
    game_over(FALSE)
    start_time(NULL)
    final_time(NULL)
    output$feedback <- renderText("")
  })
}

# Run the app
shinyApp(ui, server)
