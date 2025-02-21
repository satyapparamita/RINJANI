## Lexicon list
lex <- c("anxiety", "withdrawal", "severe", "delusions", "adhd", "weight", "insomnia", "drowsiness", "suicidal", "appetite",
         "dizziness", "nausea", "episodes", "attacks", "sleep", "seizures", "addictive", "weaned", "swings", "dysfunction",
         "blurred", "irritability", "headache", "fatigue", "imbalance", "nervousness", "psychosis", "drowsy", "fun", "play",
         "helped", "god", "answer", "wants", "leave", "beautiful", "suffer", "sorry", "tolerance", "agree", "hate", "helpful",
         "haha", "enjoy", "social", "talk", "save", "win", "care", "love", "like", "hold", "cope", "amazing", "discuss",
         "medication","side???effects","doctor","doses","effective","prescribed","therapy","inhibitor","stimulant",
         "patients","neurotransmitters","prescriptions","psychotherapy","diagnosis","clinical","pills","chemical",
         "counteract","toxicity","hospitalization","sedative","150mg","40mg","drugs","life","home","woman","she","him","girl",
         "game", "men","friends","sexual","boy","someone","movie","favorite","jesus","house","music","religion","her","songs",
         "party", "bible", "relationship","hell","young","style","church","lord","father","season","heaven","dating",
         "depress","stress","alcohol","fuck","shit","lone","support","fake","suicide","hope")

## Encode if the lexicons present on tweets (1 for yes, 0 otherwise)
res <- list()
for (i in lex) {
  res[[i]] <- as.numeric(str_detect(data$text, i))
}
res <- as.data.frame(res)

data <- cbind(data,res)

