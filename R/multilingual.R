# ============================================================
# multilingual.R  --  Multilingual UI support for SAE Dashboard
#
# Provides supported_languages() and translator() for
# runtime language switching in the Shiny dashboard.
# ============================================================

# Translation dictionary keyed by language code
.translations <- list(
  en = list(
    app_title          = "Small Area Estimation Dashboard",
    model_diagnostics  = "Model Diagnostics",
    results            = "Results",
    btn_run            = "Run Analysis",
    btn_download       = "Download Report",
    settings           = "Settings",
    language           = "Language",
    year               = "Year",
    domain             = "Domain",
    estimate           = "Estimate",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Convergence",
    normality_re       = "Normality (Random Effects)",
    normality_resid    = "Normality (Residuals)",
    llm_consent_text   = paste(
      "This dashboard can optionally use AI assistance (Claude and ChatGPT) to help",
      "interpret diagnostics and generate analysis briefs. When enabled,",
      "only aggregate summary statistics are shared -- never raw microdata.",
      "You can disable this feature at any time."
    ),
    llm_enable         = "Enable AI Assistant",
    llm_disable        = "Disable AI Assistant"
  ),

  fr = list(
    app_title          = "Tableau de Bord d'Estimation sur Petits Domaines",
    model_diagnostics  = "Diagnostics du Mod\u00e8le",
    results            = "R\u00e9sultats",
    btn_run            = "Lancer l'Analyse",
    btn_download       = "T\u00e9l\u00e9charger le Rapport",
    settings           = "Param\u00e8tres",
    language           = "Langue",
    year               = "Ann\u00e9e",
    domain             = "Domaine",
    estimate           = "Estimation",
    cv                 = "CV (%)",
    mse                = "EQM",
    convergence        = "Convergence",
    normality_re       = "Normalit\u00e9 (Effets Al\u00e9atoires)",
    normality_resid    = "Normalit\u00e9 (R\u00e9sidus)",
    llm_consent_text   = paste(
      "Ce tableau de bord peut utiliser une assistance IA (Claude et ChatGPT) pour aider",
      "\u00e0 interpr\u00e9ter les diagnostics. Seules des statistiques agr\u00e9g\u00e9es sont",
      "partag\u00e9es \u2014 jamais de micro-donn\u00e9es brutes."
    ),
    llm_enable         = "Activer l'assistant IA",
    llm_disable        = "D\u00e9sactiver l'assistant IA"
  ),

  es = list(
    app_title          = "Panel de Estimaci\u00f3n de \u00c1reas Peque\u00f1as",
    model_diagnostics  = "Diagn\u00f3sticos del Modelo",
    results            = "Resultados",
    btn_run            = "Ejecutar An\u00e1lisis",
    btn_download       = "Descargar Informe",
    settings           = "Configuraci\u00f3n",
    language           = "Idioma",
    year               = "A\u00f1o",
    domain             = "Dominio",
    estimate           = "Estimaci\u00f3n",
    cv                 = "CV (%)",
    mse                = "ECM",
    convergence        = "Convergencia",
    normality_re       = "Normalidad (Efectos Aleatorios)",
    normality_resid    = "Normalidad (Residuos)",
    llm_consent_text   = paste(
      "Este panel puede utilizar asistencia de IA (Claude y ChatGPT) para interpretar",
      "diagn\u00f3sticos. Solo se comparten estad\u00edsticas agregadas \u2014 nunca microdatos."
    ),
    llm_enable         = "Activar asistente IA",
    llm_disable        = "Desactivar asistente IA"
  ),

  el = list(
    app_title          = "\u03a0\u03af\u03bd\u03b1\u03ba\u03b1\u03c2 \u0395\u03ba\u03c4\u03af\u03bc\u03b7\u03c3\u03b7\u03c2 \u039c\u03b9\u03ba\u03c1\u03ce\u03bd \u03a0\u03b5\u03c1\u03b9\u03bf\u03c7\u03ce\u03bd",
    model_diagnostics  = "\u0394\u03b9\u03b1\u03b3\u03bd\u03c9\u03c3\u03c4\u03b9\u03ba\u03ac \u039c\u03bf\u03bd\u03c4\u03ad\u03bb\u03bf\u03c5",
    results            = "\u0391\u03c0\u03bf\u03c4\u03b5\u03bb\u03ad\u03c3\u03bc\u03b1\u03c4\u03b1",
    btn_run            = "\u0395\u03ba\u03c4\u03ad\u03bb\u03b5\u03c3\u03b7 \u0391\u03bd\u03ac\u03bb\u03c5\u03c3\u03b7\u03c2",
    btn_download       = "\u039b\u03ae\u03c8\u03b7 \u0391\u03bd\u03b1\u03c6\u03bf\u03c1\u03ac\u03c2",
    settings           = "\u03a1\u03c5\u03b8\u03bc\u03af\u03c3\u03b5\u03b9\u03c2",
    language           = "\u0393\u03bb\u03ce\u03c3\u03c3\u03b1",
    year               = "\u0388\u03c4\u03bf\u03c2",
    domain             = "\u03a0\u03b5\u03c1\u03b9\u03bf\u03c7\u03ae",
    estimate           = "\u0395\u03ba\u03c4\u03af\u03bc\u03b7\u03c3\u03b7",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "\u03a3\u03cd\u03b3\u03ba\u03bb\u03b9\u03c3\u03b7",
    normality_re       = "\u039a\u03b1\u03bd\u03bf\u03bd\u03b9\u03ba\u03cc\u03c4\u03b7\u03c4\u03b1 (\u03a4\u03c5\u03c7\u03b1\u03af\u03b1 \u0391\u03c0\u03bf\u03c4\u03b5\u03bb\u03ad\u03c3\u03bc\u03b1\u03c4\u03b1)",
    normality_resid    = "\u039a\u03b1\u03bd\u03bf\u03bd\u03b9\u03ba\u03cc\u03c4\u03b7\u03c4\u03b1 (\u03a5\u03c0\u03cc\u03bb\u03bf\u03b9\u03c0\u03b1)",
    llm_consent_text   = paste(
        "\u0391\u03c5\u03c4\u03cc\u03c2 \u03bf \u03c0\u03af\u03bd\u03b1\u03ba\u03b1\u03c2 \u03bc\u03c0\u03bf\u03c1\u03b5\u03af \u03bd\u03b1 \u03c7\u03c1\u03b7\u03c3\u03b9\u03bc\u03bf\u03c0\u03bf\u03b9\u03ae\u03c3\u03b5\u03b9 AI assistance (Claude \u03ba\u03b1\u03b9 ChatGPT) \u03b3\u03b9\u03b1 \u03c4\u03b7\u03bd",
      "\u03b5\u03c1\u03bc\u03b7\u03bd\u03b5\u03af\u03b1 \u03b4\u03b9\u03b1\u03b3\u03bd\u03c9\u03c3\u03c4\u03b9\u03ba\u03ce\u03bd. \u039a\u03bf\u03b9\u03bd\u03bf\u03c0\u03bf\u03b9\u03bf\u03cd\u03bd\u03c4\u03b1\u03b9 \u03bc\u03cc\u03bd\u03bf \u03c3\u03c5\u03b3\u03ba\u03b5\u03bd\u03c4\u03c1\u03c9\u03c4\u03b9\u03ba\u03ac \u03c3\u03c4\u03b1\u03c4\u03b9\u03c3\u03c4\u03b9\u03ba\u03ac",
      "\u2014 \u03c0\u03bf\u03c4\u03ad \u03bc\u03b9\u03ba\u03c1\u03bf\u03b4\u03b5\u03b4\u03bf\u03bc\u03ad\u03bd\u03b1."
    ),
    llm_enable         = "\u0395\u03bd\u03b5\u03c1\u03b3\u03bf\u03c0\u03bf\u03af\u03b7\u03c3\u03b7 \u03b2\u03bf\u03b7\u03b8\u03bf\u03cd AI",
    llm_disable        = "\u0391\u03c0\u03b5\u03bd\u03b5\u03c1\u03b3\u03bf\u03c0\u03bf\u03af\u03b7\u03c3\u03b7 \u03b2\u03bf\u03b7\u03b8\u03bf\u03cd AI"
  ),

  ar = list(
    app_title          = "\u0644\u0648\u062d\u0629 \u062a\u0642\u062f\u064a\u0631 \u0627\u0644\u0645\u0646\u0627\u0637\u0642 \u0627\u0644\u0635\u063a\u064a\u0631\u0629",
    model_diagnostics  = "\u062a\u0634\u062e\u064a\u0635\u0627\u062a \u0627\u0644\u0646\u0645\u0648\u0630\u062c",
    results            = "\u0627\u0644\u0646\u062a\u0627\u0626\u062c",
    btn_run            = "\u062a\u0634\u063a\u064a\u0644 \u0627\u0644\u062a\u062d\u0644\u064a\u0644",
    btn_download       = "\u062a\u062d\u0645\u064a\u0644 \u0627\u0644\u062a\u0642\u0631\u064a\u0631",
    settings           = "\u0627\u0644\u0625\u0639\u062f\u0627\u062f\u0627\u062a",
    language           = "\u0627\u0644\u0644\u063a\u0629",
    year               = "\u0627\u0644\u0633\u0646\u0629",
    domain             = "\u0627\u0644\u0645\u0646\u0637\u0642\u0629",
    estimate           = "\u0627\u0644\u062a\u0642\u062f\u064a\u0631",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "\u0627\u0644\u062a\u0642\u0627\u0631\u0628",
    normality_re       = "\u0627\u0644\u0637\u0628\u064a\u0639\u064a\u0629 (\u0627\u0644\u062a\u0623\u062b\u064a\u0631\u0627\u062a \u0627\u0644\u0639\u0634\u0648\u0627\u0626\u064a\u0629)",
    normality_resid    = "\u0627\u0644\u0637\u0628\u064a\u0639\u064a\u0629 (\u0627\u0644\u0628\u0648\u0627\u0642\u064a)",
    llm_consent_text   = paste(
        "\u064a\u0645\u0643\u0646 \u0644\u0647\u0630\u0647 \u0627\u0644\u0644\u0648\u062d\u0629 \u0627\u0633\u062a\u062e\u062f\u0627\u0645 \u0645\u0633\u0627\u0639\u062f\u0629 \u0630\u0643\u0627\u0621 \u0627\u0635\u0637\u0646\u0627\u0639\u064a (Claude \u0648 ChatGPT) \u0644\u0644\u0645\u0633\u0627\u0639\u062f\u0629",
      "\u0641\u064a \u062a\u0641\u0633\u064a\u0631 \u0627\u0644\u062a\u0634\u062e\u064a\u0635\u0627\u062a. \u062a\u062a\u0645 \u0645\u0634\u0627\u0631\u0643\u0629 \u0627\u0644\u0625\u062d\u0635\u0627\u0621\u0627\u062a \u0627\u0644\u0645\u062c\u0645\u0639\u0629 \u0641\u0642\u0637 \u2014 \u0644\u0627 \u0628\u064a\u0627\u0646\u0627\u062a \u062c\u0632\u0626\u064a\u0629 \u0623\u0628\u062f\u064b\u0627."
    ),
    llm_enable         = "\u062a\u0641\u0639\u064a\u0644 \u0645\u0633\u0627\u0639\u062f AI",
    llm_disable        = "\u062a\u0639\u0637\u064a\u0644 \u0645\u0633\u0627\u0639\u062f AI"
  ),

  # ---- EU official languages (additional) ----

  bg = list(
    app_title          = "\u0422\u0430\u0431\u043b\u043e \u0437\u0430 \u043e\u0446\u0435\u043d\u043a\u0430 \u043d\u0430 \u043c\u0430\u043b\u043a\u0438 \u0440\u0430\u0439\u043e\u043d\u0438",
    model_diagnostics  = "\u0414\u0438\u0430\u0433\u043d\u043e\u0441\u0442\u0438\u043a\u0430 \u043d\u0430 \u043c\u043e\u0434\u0435\u043b\u0430",
    results            = "\u0420\u0435\u0437\u0443\u043b\u0442\u0430\u0442\u0438",
    btn_run            = "\u0421\u0442\u0430\u0440\u0442\u0438\u0440\u0430\u043d\u0435 \u043d\u0430 \u0430\u043d\u0430\u043b\u0438\u0437\u0430",
    btn_download       = "\u0418\u0437\u0442\u0435\u0433\u043b\u044f\u043d\u0435 \u043d\u0430 \u0434\u043e\u043a\u043b\u0430\u0434\u0430",
    settings           = "\u041d\u0430\u0441\u0442\u0440\u043e\u0439\u043a\u0438",
    language           = "\u0415\u0437\u0438\u043a",
    year               = "\u0413\u043e\u0434\u0438\u043d\u0430",
    domain             = "\u0420\u0430\u0439\u043e\u043d",
    estimate           = "\u041e\u0446\u0435\u043d\u043a\u0430",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "\u041a\u043e\u043d\u0432\u0435\u0440\u0433\u0435\u043d\u0446\u0438\u044f",
    normality_re       = "\u041d\u043e\u0440\u043c\u0430\u043b\u043d\u043e\u0441\u0442 (\u0421\u043b\u0443\u0447\u0430\u0439\u043d\u0438 \u0435\u0444\u0435\u043a\u0442\u0438)",
    normality_resid    = "\u041d\u043e\u0440\u043c\u0430\u043b\u043d\u043e\u0441\u0442 (\u041e\u0441\u0442\u0430\u0442\u044a\u0446\u0438)",
    llm_consent_text   = paste(
      "\u0422\u043e\u0432\u0430 \u0442\u0430\u0431\u043b\u043e \u043c\u043e\u0436\u0435 \u0434\u0430 \u0438\u0437\u043f\u043e\u043b\u0437\u0432\u0430 AI \u043f\u043e\u043c\u043e\u0449 (Claude \u0438 ChatGPT) \u0437\u0430 \u0438\u043d\u0442\u0435\u0440\u043f\u0440\u0435\u0442\u0430\u0446\u0438\u044f",
      "\u043d\u0430 \u0434\u0438\u0430\u0433\u043d\u043e\u0441\u0442\u0438\u043a\u0438. \u0421\u043f\u043e\u0434\u0435\u043b\u044f\u0442 \u0441\u0435 \u0441\u0430\u043c\u043e \u0430\u0433\u0440\u0435\u0433\u0438\u0440\u0430\u043d\u0438 \u0441\u0442\u0430\u0442\u0438\u0441\u0442\u0438\u043a\u0438 \u2014 \u043d\u0438\u043a\u043e\u0433\u0430 \u043c\u0438\u043a\u0440\u043e\u0434\u0430\u043d\u043d\u0438."
    ),
    llm_enable         = "\u0410\u043a\u0442\u0438\u0432\u0438\u0440\u0430\u043d\u0435 \u043d\u0430 AI \u043f\u043e\u043c\u043e\u0449\u043d\u0438\u043a",
    llm_disable        = "\u0414\u0435\u0430\u043a\u0442\u0438\u0432\u0438\u0440\u0430\u043d\u0435 \u043d\u0430 AI \u043f\u043e\u043c\u043e\u0449\u043d\u0438\u043a"
  ),

  hr = list(
    app_title          = "Nadzorna plo\u010da za procjenu malih podru\u010dja",
    model_diagnostics  = "Dijagnostika modela",
    results            = "Rezultati",
    btn_run            = "Pokreni analizu",
    btn_download       = "Preuzmi izvje\u0161taj",
    settings           = "Postavke",
    language           = "Jezik",
    year               = "Godina",
    domain             = "Podru\u010dje",
    estimate           = "Procjena",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konvergencija",
    normality_re       = "Normalnost (Slu\u010dajni efekti)",
    normality_resid    = "Normalnost (Reziduali)",
    llm_consent_text   = paste(
      "Ova nadzorna plo\u010da mo\u017ee koristiti AI pomo\u0107 (Claude i ChatGPT) za",
      "interpretaciju dijagnostike. Dijele se samo agregirane statistike \u2014 nikada mikropodaci."
    ),
    llm_enable         = "Omogu\u0107i AI asistenta",
    llm_disable        = "Onemogu\u0107i AI asistenta"
  ),

  cs = list(
    app_title          = "Panel odhadu mal\u00fdch oblast\u00ed",
    model_diagnostics  = "Diagnostika modelu",
    results            = "V\u00fdsledky",
    btn_run            = "Spustit anal\u00fdzu",
    btn_download       = "St\u00e1hnout zpr\u00e1vu",
    settings           = "Nastaven\u00ed",
    language           = "Jazyk",
    year               = "Rok",
    domain             = "Oblast",
    estimate           = "Odhad",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konvergence",
    normality_re       = "Normalita (N\u00e1hodn\u00e9 efekty)",
    normality_resid    = "Normalita (Rezidua)",
    llm_consent_text   = paste(
      "Tento panel m\u016f\u017ee vyu\u017e\u00edvat AI asistenci (Claude a ChatGPT) k interpretaci",
      "diagnostiky. Sd\u00edlej\u00ed se pouze agregovan\u00e9 statistiky \u2014 nikdy mikrodata."
    ),
    llm_enable         = "Povolit AI asistenta",
    llm_disable        = "Zak\u00e1zat AI asistenta"
  ),

  da = list(
    app_title          = "Dashboard til sm\u00e5omr\u00e5deestimering",
    model_diagnostics  = "Modeldiagnostik",
    results            = "Resultater",
    btn_run            = "K\u00f8r analyse",
    btn_download       = "Download rapport",
    settings           = "Indstillinger",
    language           = "Sprog",
    year               = "\u00c5r",
    domain             = "Omr\u00e5de",
    estimate           = "Estimat",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konvergens",
    normality_re       = "Normalitet (Tilf\u00e6ldige effekter)",
    normality_resid    = "Normalitet (Residualer)",
    llm_consent_text   = paste(
      "Dette dashboard kan bruge AI-assistance (Claude og ChatGPT) til at fortolke",
      "diagnostik. Kun aggregerede statistikker deles \u2014 aldrig mikrodata."
    ),
    llm_enable         = "Aktiv\u00e9r AI-assistent",
    llm_disable        = "Deaktiv\u00e9r AI-assistent"
  ),

  nl = list(
    app_title          = "Dashboard voor kleine-gebiedsschattingen",
    model_diagnostics  = "Modeldiagnostiek",
    results            = "Resultaten",
    btn_run            = "Analyse uitvoeren",
    btn_download       = "Rapport downloaden",
    settings           = "Instellingen",
    language           = "Taal",
    year               = "Jaar",
    domain             = "Gebied",
    estimate           = "Schatting",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Convergentie",
    normality_re       = "Normaliteit (Willekeurige effecten)",
    normality_resid    = "Normaliteit (Residuen)",
    llm_consent_text   = paste(
      "Dit dashboard kan AI-assistentie (Claude en ChatGPT) gebruiken voor de interpretatie",
      "van diagnostiek. Alleen geaggregeerde statistieken worden gedeeld \u2014 nooit microdata."
    ),
    llm_enable         = "AI-assistent inschakelen",
    llm_disable        = "AI-assistent uitschakelen"
  ),

  et = list(
    app_title          = "V\u00e4ikeste piirkondade hindamise t\u00f6\u00f6laud",
    model_diagnostics  = "Mudeli diagnostika",
    results            = "Tulemused",
    btn_run            = "K\u00e4ivita anal\u00fc\u00fcs",
    btn_download       = "Laadi aruanne alla",
    settings           = "Seaded",
    language           = "Keel",
    year               = "Aasta",
    domain             = "Piirkond",
    estimate           = "Hinnang",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Koondumine",
    normality_re       = "Normaalsus (Juhuslikud efektid)",
    normality_resid    = "Normaalsus (J\u00e4\u00e4gid)",
    llm_consent_text   = paste(
      "See t\u00f6\u00f6laud saab kasutada AI abi (Claude ja ChatGPT) diagnostika",
      "t\u00f5lgendamiseks. Jagatakse ainult koondstatistikat \u2014 mitte kunagi mikroandmeid."
    ),
    llm_enable         = "Luba AI assistent",
    llm_disable        = "Keela AI assistent"
  ),

  fi = list(
    app_title          = "Pienalue-estimoinnin kojelauta",
    model_diagnostics  = "Mallidiagnostiikka",
    results            = "Tulokset",
    btn_run            = "Suorita analyysi",
    btn_download       = "Lataa raportti",
    settings           = "Asetukset",
    language           = "Kieli",
    year               = "Vuosi",
    domain             = "Alue",
    estimate           = "Estimaatti",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konvergenssi",
    normality_re       = "Normaalisuus (Satunnaisvaikutukset)",
    normality_resid    = "Normaalisuus (Residuaalit)",
    llm_consent_text   = paste(
      "T\u00e4m\u00e4 kojelauta voi k\u00e4ytt\u00e4\u00e4 teko\u00e4lyapua (Claude ja ChatGPT) diagnostiikan",
      "tulkintaan. Vain koottuja tilastoja jaetaan \u2014 ei koskaan mikroaineistoja."
    ),
    llm_enable         = "Ota AI-avustaja k\u00e4ytt\u00f6\u00f6n",
    llm_disable        = "Poista AI-avustaja k\u00e4yt\u00f6st\u00e4"
  ),

  de = list(
    app_title          = "Dashboard f\u00fcr Sch\u00e4tzung kleiner Gebiete",
    model_diagnostics  = "Modelldiagnostik",
    results            = "Ergebnisse",
    btn_run            = "Analyse starten",
    btn_download       = "Bericht herunterladen",
    settings           = "Einstellungen",
    language           = "Sprache",
    year               = "Jahr",
    domain             = "Gebiet",
    estimate           = "Sch\u00e4tzung",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konvergenz",
    normality_re       = "Normalit\u00e4t (Zuf\u00e4llige Effekte)",
    normality_resid    = "Normalit\u00e4t (Residuen)",
    llm_consent_text   = paste(
      "Dieses Dashboard kann KI-Unterst\u00fctzung (Claude und ChatGPT) zur Interpretation",
      "von Diagnostiken nutzen. Es werden nur aggregierte Statistiken geteilt \u2014 niemals Mikrodaten."
    ),
    llm_enable         = "KI-Assistent aktivieren",
    llm_disable        = "KI-Assistent deaktivieren"
  ),

  hu = list(
    app_title          = "Kister\u00fcleti becsl\u00e9s vez\u00e9rl\u0151pult",
    model_diagnostics  = "Modelldiagnosztika",
    results            = "Eredm\u00e9nyek",
    btn_run            = "Elemz\u00e9s ind\u00edt\u00e1sa",
    btn_download       = "Jelent\u00e9s let\u00f6lt\u00e9se",
    settings           = "Be\u00e1ll\u00edt\u00e1sok",
    language           = "Nyelv",
    year               = "\u00c9v",
    domain             = "Ter\u00fclet",
    estimate           = "Becsl\u00e9s",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konvergencia",
    normality_re       = "Normalit\u00e1s (V\u00e9letlen hat\u00e1sok)",
    normality_resid    = "Normalit\u00e1s (Reziduumok)",
    llm_consent_text   = paste(
      "Ez a vez\u00e9rl\u0151pult AI seg\u00edts\u00e9get (Claude \u00e9s ChatGPT) haszn\u00e1lhat a",
      "diagnosztika \u00e9rtelmez\u00e9s\u00e9hez. Csak \u00f6sszess\u00edtett statisztik\u00e1k ker\u00fclnek megoszt\u00e1sra \u2014 soha nem mikroadatok."
    ),
    llm_enable         = "AI asszisztens enged\u00e9lyez\u00e9se",
    llm_disable        = "AI asszisztens letilt\u00e1sa"
  ),

  ga = list(
    app_title          = "Paineal Meast\u00f3ireachta Limist\u00e9ar Beag",
    model_diagnostics  = "Diagn\u00f3isic an Mh\u00fail\u00ed",
    results            = "Torthai",
    btn_run            = "Rith Anail\u00eds",
    btn_download       = "\u00cdosluchtaigh Tuarasc\u00e1il",
    settings           = "Socruithe",
    language           = "Teanga",
    year               = "Bliain",
    domain             = "Fear\u00e1n",
    estimate           = "Meast\u00e1ch\u00e1n",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "C\u00f3ineasacht",
    normality_re       = "Gn\u00e1th\u00falacht (\u00c9ifeachta\u00ed Randamacha)",
    normality_resid    = "Gn\u00e1th\u00falacht (Iarmh\u00e9id)",
    llm_consent_text   = paste(
      "Is f\u00e9idir leis an bpaineal seo c\u00fanamh AI (Claude agus ChatGPT) a \u00fas\u00e1id chun",
      "diagn\u00f3isic\u00ed a l\u00e9irmh\u00edni\u00fa. N\u00ed roinntear ach staitistic\u00ed comhioml\u00e1nacha \u2014 n\u00ed micra-shonra\u00ed riamh."
    ),
    llm_enable         = "Cumasaigh c\u00fant\u00f3ir AI",
    llm_disable        = "D\u00edchumasaigh c\u00fant\u00f3ir AI"
  ),

  it = list(
    app_title          = "Pannello di Stima per Piccole Aree",
    model_diagnostics  = "Diagnostica del Modello",
    results            = "Risultati",
    btn_run            = "Esegui Analisi",
    btn_download       = "Scarica Rapporto",
    settings           = "Impostazioni",
    language           = "Lingua",
    year               = "Anno",
    domain             = "Dominio",
    estimate           = "Stima",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Convergenza",
    normality_re       = "Normalit\u00e0 (Effetti Casuali)",
    normality_resid    = "Normalit\u00e0 (Residui)",
    llm_consent_text   = paste(
      "Questo pannello pu\u00f2 utilizzare l'assistenza AI (Claude e ChatGPT) per interpretare",
      "la diagnostica. Vengono condivise solo statistiche aggregate \u2014 mai microdati."
    ),
    llm_enable         = "Attiva assistente AI",
    llm_disable        = "Disattiva assistente AI"
  ),

  lv = list(
    app_title          = "Mazo apgabalu nov\u0113rt\u0113juma panelis",
    model_diagnostics  = "Mode\u013ca diagnostika",
    results            = "Rezult\u0101ti",
    btn_run            = "Palaist anal\u012bzi",
    btn_download       = "Lejupiel\u0101d\u0113t atskaiti",
    settings           = "Iestat\u012bjumi",
    language           = "Valoda",
    year               = "Gads",
    domain             = "Apgabals",
    estimate           = "Nov\u0113rt\u0113jums",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konver\u0123ence",
    normality_re       = "Normalit\u0101te (Nejau\u0161ie efekti)",
    normality_resid    = "Normalit\u0101te (Atlikumi)",
    llm_consent_text   = paste(
      "\u0160is panelis var izmantot AI pal\u012bdz\u012bbu (Claude un ChatGPT) diagnostikas",
      "interpret\u0101cijai. Tiek kop\u012bgoti tikai apkopoti statistikas dati \u2014 nekad mikrodati."
    ),
    llm_enable         = "Iesp\u0113jot AI asistentu",
    llm_disable        = "Atsp\u0113jot AI asistentu"
  ),

  lt = list(
    app_title          = "Ma\u017e\u0173 sri\u010di\u0173 vertinimo skydelis",
    model_diagnostics  = "Modelio diagnostika",
    results            = "Rezultatai",
    btn_run            = "Vykdyti analiz\u0119",
    btn_download       = "Atsisi\u0173sti ataskait\u0105",
    settings           = "Nustatymai",
    language           = "Kalba",
    year               = "Metai",
    domain             = "Sritis",
    estimate           = "\u012evertis",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konvergencija",
    normality_re       = "Normalumas (Atsitiktiniai efektai)",
    normality_resid    = "Normalumas (Liekanos)",
    llm_consent_text   = paste(
      "\u0160is skydelis gali naudoti AI pagalb\u0105 (Claude ir ChatGPT) diagnostikos",
      "interpretavimui. Dalijamasi tik agreguota statistika \u2014 niekada mikroduomenimis."
    ),
    llm_enable         = "\u012ejungti AI asistent\u0105",
    llm_disable        = "I\u0161jungti AI asistent\u0105"
  ),

  mt = list(
    app_title          = "Dashboard g\u0127all-Istima ta' \u017bonot \u017bg\u0127ar",
    model_diagnostics  = "Dijanjostika tal-Mudell",
    results            = "Ri\u017cultati",
    btn_run            = "Mexxi l-Anali\u017ci",
    btn_download       = "Ni\u017c\u017cel ir-Rapport",
    settings           = "Settings",
    language           = "Lingwa",
    year               = "Sena",
    domain             = "\u017bona",
    estimate           = "Stima",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konver\u0121enza",
    normality_re       = "Normalit\u00e0 (Effetti Ka\u017cwali)",
    normality_resid    = "Normalit\u00e0 (Residwi)",
    llm_consent_text   = paste(
      "Dan id-dashboard jista' ju\u017ca assistenza AI (Claude u ChatGPT) biex jinterpreta",
      "d-dijanjostika. Jin\u0127admu biss statistika aggregata \u2014 qatt mikrodata."
    ),
    llm_enable         = "Attiva assistent AI",
    llm_disable        = "Iddi\u017cattiva assistent AI"
  ),

  pl = list(
    app_title          = "Panel szacowania ma\u0142ych obszar\u00f3w",
    model_diagnostics  = "Diagnostyka modelu",
    results            = "Wyniki",
    btn_run            = "Uruchom analiz\u0119",
    btn_download       = "Pobierz raport",
    settings           = "Ustawienia",
    language           = "J\u0119zyk",
    year               = "Rok",
    domain             = "Obszar",
    estimate           = "Oszacowanie",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konwergencja",
    normality_re       = "Normalno\u015b\u0107 (Efekty losowe)",
    normality_resid    = "Normalno\u015b\u0107 (Reszty)",
    llm_consent_text   = paste(
      "Ten panel mo\u017ce korzysta\u0107 z asystenta AI (Claude i ChatGPT) do interpretacji",
      "diagnostyki. Udost\u0119pniane s\u0105 wy\u0142\u0105cznie zagregowane statystyki \u2014 nigdy mikrodane."
    ),
    llm_enable         = "W\u0142\u0105cz asystenta AI",
    llm_disable        = "Wy\u0142\u0105cz asystenta AI"
  ),

  pt = list(
    app_title          = "Painel de Estima\u00e7\u00e3o de Pequenas \u00c1reas",
    model_diagnostics  = "Diagn\u00f3sticos do Modelo",
    results            = "Resultados",
    btn_run            = "Executar An\u00e1lise",
    btn_download       = "Descarregar Relat\u00f3rio",
    settings           = "Defini\u00e7\u00f5es",
    language           = "Idioma",
    year               = "Ano",
    domain             = "Dom\u00ednio",
    estimate           = "Estimativa",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Converg\u00eancia",
    normality_re       = "Normalidade (Efeitos Aleat\u00f3rios)",
    normality_resid    = "Normalidade (Res\u00edduos)",
    llm_consent_text   = paste(
      "Este painel pode utilizar assist\u00eancia de IA (Claude e ChatGPT) para interpretar",
      "diagn\u00f3sticos. Apenas estat\u00edsticas agregadas s\u00e3o partilhadas \u2014 nunca microdados."
    ),
    llm_enable         = "Ativar assistente IA",
    llm_disable        = "Desativar assistente IA"
  ),

  ro = list(
    app_title          = "Panou de Estimare a Zonelor Mici",
    model_diagnostics  = "Diagnosticarea Modelului",
    results            = "Rezultate",
    btn_run            = "Ruleaz\u0103 Analiza",
    btn_download       = "Descarc\u0103 Raportul",
    settings           = "Set\u0103ri",
    language           = "Limb\u0103",
    year               = "An",
    domain             = "Domeniu",
    estimate           = "Estimare",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Convergen\u021b\u0103",
    normality_re       = "Normalitate (Efecte Aleatorii)",
    normality_resid    = "Normalitate (Reziduuri)",
    llm_consent_text   = paste(
      "Acest panou poate utiliza asisten\u021b\u0103 AI (Claude \u0219i ChatGPT) pentru interpretarea",
      "diagnosticelor. Se partajeaz\u0103 doar statistici agregate \u2014 niciodat\u0103 microdate."
    ),
    llm_enable         = "Activeaz\u0103 asistentul AI",
    llm_disable        = "Dezactiveaz\u0103 asistentul AI"
  ),

  sk = list(
    app_title          = "Panel odhadu mal\u00fdch oblast\u00ed",
    model_diagnostics  = "Diagnostika modelu",
    results            = "V\u00fdsledky",
    btn_run            = "Spusti\u0165 anal\u00fdzu",
    btn_download       = "Stiahnu\u0165 spr\u00e1vu",
    settings           = "Nastavenia",
    language           = "Jazyk",
    year               = "Rok",
    domain             = "Oblas\u0165",
    estimate           = "Odhad",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konvergencia",
    normality_re       = "Normalita (N\u00e1hodn\u00e9 efekty)",
    normality_resid    = "Normalita (Rezidu\u00e1ly)",
    llm_consent_text   = paste(
      "Tento panel m\u00f4\u017ee vyu\u017e\u00edva\u0165 AI asistenciu (Claude a ChatGPT) na interpret\u00e1ciu",
      "diagnostiky. Zdie\u013eaj\u00fa sa len agregovan\u00e9 \u0161tatistiky \u2014 nikdy mikrodata."
    ),
    llm_enable         = "Povoli\u0165 AI asistenta",
    llm_disable        = "Zak\u00e1za\u0165 AI asistenta"
  ),

  sl = list(
    app_title          = "Nadzorna plo\u0161\u010da za ocenjevanje majhnih obmo\u010dij",
    model_diagnostics  = "Diagnostika modela",
    results            = "Rezultati",
    btn_run            = "Za\u017eeni analizo",
    btn_download       = "Prenesi poro\u010dilo",
    settings           = "Nastavitve",
    language           = "Jezik",
    year               = "Leto",
    domain             = "Obmo\u010dje",
    estimate           = "Ocena",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konvergenca",
    normality_re       = "Normalnost (Naklju\u010dni u\u010dinki)",
    normality_resid    = "Normalnost (Residuali)",
    llm_consent_text   = paste(
      "Ta nadzorna plo\u0161\u010da lahko uporablja pomo\u010d umetne inteligence (Claude in ChatGPT) za",
      "interpretacijo diagnostike. Deljene so le agregirane statistike \u2014 nikoli mikropodatki."
    ),
    llm_enable         = "Omogo\u010di AI pomo\u010dnika",
    llm_disable        = "Onemogo\u010di AI pomo\u010dnika"
  ),

  sv = list(
    app_title          = "Panel f\u00f6r sm\u00e5omr\u00e5desskattning",
    model_diagnostics  = "Modelldiagnostik",
    results            = "Resultat",
    btn_run            = "K\u00f6r analys",
    btn_download       = "Ladda ner rapport",
    settings           = "Inst\u00e4llningar",
    language           = "Spr\u00e5k",
    year               = "\u00c5r",
    domain             = "Omr\u00e5de",
    estimate           = "Skattning",
    cv                 = "CV (%)",
    mse                = "MSE",
    convergence        = "Konvergens",
    normality_re       = "Normalitet (Slumpm\u00e4ssiga effekter)",
    normality_resid    = "Normalitet (Residualer)",
    llm_consent_text   = paste(
      "Denna panel kan anv\u00e4nda AI-assistans (Claude och ChatGPT) f\u00f6r att tolka",
      "diagnostik. Endast aggregerad statistik delas \u2014 aldrig mikrodata."
    ),
    llm_enable         = "Aktivera AI-assistent",
    llm_disable        = "Inaktivera AI-assistent"
  )
)

#' List supported languages
#'
#' @return Named character vector (code = label)
supported_languages <- function() {
  c("English"                                        = "en",
    "Fran\u00e7ais"                                  = "fr",
    "Deutsch"                                        = "de",
    "Espa\u00f1ol"                                   = "es",
    "Italiano"                                       = "it",
    "Portugu\u00eas"                                 = "pt",
    "Nederlands"                                     = "nl",
    "Polski"                                         = "pl",
    "Rom\u00e2n\u0103"                               = "ro",
    "\u010ce\u0161tina"                              = "cs",
    "Sloven\u010dina"                                = "sk",
    "Sloven\u0161\u010dina"                          = "sl",
    "Magyar"                                         = "hu",
    "Svenska"                                        = "sv",
    "Dansk"                                          = "da",
    "Suomi"                                          = "fi",
    "Eesti"                                          = "et",
    "Latvie\u0161u"                                  = "lv",
    "Lietuvi\u0173"                                  = "lt",
    "Malti"                                          = "mt",
    "Gaeilge"                                        = "ga",
    "Hrvatski"                                       = "hr",
    "\u0411\u044a\u043b\u0433\u0430\u0440\u0441\u043a\u0438" = "bg",
    "\u0395\u03bb\u03bb\u03b7\u03bd\u03b9\u03ba\u03ac"       = "el",
    "\u0627\u0644\u0639\u0631\u0628\u064a\u0629"             = "ar")
}

#' Create a translator for the given language
#'
#' @param lang Language code (e.g. "en", "fr", "el")
#' @return List with a $get(key) method that returns the translated string
translator <- function(lang = "en") {
  if (!lang %in% names(.translations)) {
    warning(sprintf("Language '%s' not supported, falling back to English", lang))
    lang <- "en"
  }

  dict <- .translations[[lang]]
  fallback <- .translations[["en"]]

  list(
    lang = lang,
    get  = function(key) {
      val <- dict[[key]]
      if (is.null(val)) val <- fallback[[key]]
      if (is.null(val)) val <- paste0("[", key, "]")
      val
    }
  )
}
