import requests
import re

import os
from dotenv import load_dotenv

load_dotenv("secrets.env")
API_KEY = os.getenv("OPENROUTER_API_KEY")

if not API_KEY:
    raise ValueError("❌ Clé API manquante. Assure-toi que 'secrets.env' contient OPENROUTER_API_KEY=...")

MODEL = "mistralai/mistral-7b-instruct"
PDF_PATH = "pipeline/Team 12 - Eli Awtrey - EA - A mixed-effects model of race and player penalization in sports FINAL.pdf"
CODE_PATH = "pipeline/rawTeam12code.txt"

from PyPDF2 import PdfReader

def extract_results_section(pdf_path):
    reader = PdfReader(pdf_path)
    full_text = ""
    for page in reader.pages:
        full_text += page.extract_text() + "\n"

    # Extraction de la section Results à Conclusion
    match = re.search(r"Results(.*?)Conclusion", full_text, re.DOTALL | re.IGNORECASE)
    if match:
        return match.group(1).strip()
    else:
        return "Section Results introuvable"

def safe_read(path, max_chars):
    with open(path, "r", encoding="utf-8", errors="ignore") as f:
        return f.read()[:max_chars]

# Extraction du contenu ciblé
results_text = extract_results_section(PDF_PATH)
code_content = safe_read(CODE_PATH, 5000)

# Construction du prompt
prompt = f"""
Voici un bout du code de l'équipe qui travaille sur un sujet je veux qu'à l'aide du pdf tu reconstruises le reste du code en R.

Pour information voici le code pour te mettre dans le bon repo et pour accéder au csv :

setwd('/home/mrenzo/many-analysts-variations-revisit/')
data <- read.csv(file="data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

le csv ressemble à cela  :

"playerShort","player","club","leagueCountry","birthday","height","weight","position","games","victories","ties","defeats","goals","yellowCards","yellowReds","redCards","photoID","rater1","rater2","refNum","refCountry","Alpha_3","meanIAT","nIAT","seIAT","meanExp","nExp","seExp"
"lucas-wilchez","Lucas Wilchez","Real Zaragoza","Spain","31.08.1983",177,72,"Attacking Midfielder",1,0,0,1,0,0,0,0,"95212.jpg",0.25,0.5,1,1,"GRC",0.326,712,0.0005,0.396,750,0.0026
"john-utaka","John Utaka","Montpellier HSC","France","08.01.1982",179,82,"Right Winger",1,0,0,1,0,1,0,0,"1663.jpg",0.75,0.75,2,2,"ZMB",0.203,40,0.0108,-0.204,49,0.0615

Le csv a les valeurs de ratings entre 0 et 1 mais il faut que tu les mettes entre 1 et 5 avant de faire tes calculs pour le modèle 1 et 2 mais tu peux garder le 0 à 1 pour le modèle 3 et 4.

Voici la section "Results" du PDF :
{results_text}

Voici le code existant :
{code_content}

Donne juste le code pas de commentaire rien juste le code sans commentaire
"""

# Requête à l’API OpenRouter
headers = {
    "Authorization": f"Bearer {API_KEY}",
    "Content-Type": "application/json",
}

data = {
    "model": MODEL,
    "messages": [{"role": "user", "content": prompt}],
    "max_tokens": 1024
}
# sauvegarde dans un txt prompt
with open("prompt.txt", "w", encoding="utf-8") as f:
    f.write(prompt)
response = requests.post("https://openrouter.ai/api/v1/chat/completions", headers=headers, json=data)

try:
    reply = response.json()["choices"][0]["message"]["content"]
    with open("reconstruction_modele.R", "w", encoding="utf-8") as f:
        f.write(reply)
    print("✅ Code généré enregistré dans reconstruction_modele.R")
except KeyError:
    print("⚠️ Erreur API détectée :")
    print("Statut:", response.status_code)
    print(response.text)
