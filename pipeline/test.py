import os
import re
import requests
from dotenv import load_dotenv
from PyPDF2 import PdfReader

# === CHARGEMENT DE LA CLÉ API SÉCURISÉE ===
load_dotenv("secrets.env")
API_KEY = os.getenv("OPENROUTER_API_KEY")
if not API_KEY:
    raise ValueError("❌ Clé API manquante. Vérifie le fichier secrets.env")

# === CHEMINS DE FICHIERS ===
PDF_PATH = "pipeline/Team 12 - Eli Awtrey - EA - A mixed-effects model of race and player penalization in sports FINAL.pdf"
CODE_PATH = "pipeline/rawTeam12code.txt"  # à adapter si besoin
MODEL = "mistralai/mistral-7b-instruct"
MAX_PROMPT_CHARS = 10000

# === LECTURE CIBLÉE DU PDF ===
def extract_results_section(pdf_path):
    reader = PdfReader(pdf_path)
    text = "\n".join(page.extract_text() for page in reader.pages if page.extract_text())
    match = re.search(r"Results(.*?)Conclusion", text, re.DOTALL | re.IGNORECASE)
    return match.group(1).strip() if match else "Section Results introuvable"

# === LECTURE PARTIELLE DU CODE EXISTANT ===
def read_limited(path, max_chars=5000):
    with open(path, "r", encoding="utf-8", errors="ignore") as f:
        return f.read()[:max_chars]

# === PRÉPARATION DU PROMPT ===
results_text = extract_results_section(PDF_PATH)
code_content = read_limited(CODE_PATH)

prompt = f"""
Voici un bout du code qui n'est pas complet il'y a peut etre du code en plus non divulgué de l'équipe qui travaille sur un sujet je veux qu'à l'aide du pdf tu reconstruises le code en entier avec tout ce que le pdf nous donne comme informations.

Pour information voici le code pour te mettre dans le bon repo et pour accéder au csv :

setwd('/home/mrenzo/many-analysts-variations-revisit/')
data <- read.csv(file="data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

le csv a les valeurs de ratings entre 0 et 1 mais il faut que tu les mettes entre 1 et 5 avant de faire tes calculs pour le modele 1 et 2 mais tu peux garder le 0 à 1 pour le modele 3 et 4.

Voici la section Results du PDF :
{results_text}

Voici le code existant :
{code_content}
"""

headers = {
    "Authorization": f"Bearer {API_KEY}",
    "Content-Type": "application/json",
}

data = {
    "model": MODEL,
    "messages": [{"role": "user", "content": prompt}],
    "max_tokens": 1024
}

response = requests.post("https://openrouter.ai/api/v1/chat/completions", headers=headers, json=data)

# === TRAITEMENT DE LA RÉPONSE ===
try:
    message = response.json()["choices"][0]["message"]["content"]

    # EXTRACTION DU BLOC DE CODE UNIQUEMENT
    code_match = re.search(r"```(\w+)?\n(.*?)```", message, re.DOTALL)
    if code_match:
        lang = code_match.group(1) or "txt"
        code_block = code_match.group(2).strip()
    else:
        lang = "txt"
        code_block = message.strip()

    extension = {
        "r": "R",
        "python": "py",
        "py": "py",
        "bash": "sh",
        "sh": "sh"
    }.get(lang.lower(), "txt")

    output_filename = f"reconstruction_modele.{extension}"
    with open(output_filename, "w", encoding="utf-8") as f:
        f.write(code_block)

    print(f"✅ Code extrait et sauvegardé dans {output_filename}")

except Exception as e:
    print("⚠️ Erreur :", e)
    print("Réponse brute :", response.status_code, response.text)

