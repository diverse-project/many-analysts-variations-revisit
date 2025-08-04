import os
import re
import requests
import pytesseract
from dotenv import load_dotenv
from pdf2image import convert_from_path
from PyPDF2 import PdfReader

# === CHARGEMENT DE LA CLÉ API SÉCURISÉE ===
load_dotenv("secrets.env")
API_KEY = os.getenv("OPENROUTER_API_KEY")
if not API_KEY:
    raise ValueError("Clé API manquante. check secrets.env")

# === CHEMINS DE FICHIERS ===
PDF_PATH = "pipeline/Team3.pdf"
CODE_PATH = "pipeline/rawTeam3code.txt"
MODEL = "deepseek/deepseek-chat-v3-0324:free"

# === FONCTION PDF → TEXTE AVEC OCR ===
def extract_text_via_ocr(pdf_path, dpi=300):
    print("🔍 Conversion du PDF en images...")
    images = convert_from_path(pdf_path, dpi=dpi)
    ocr_text = ""
    for i, image in enumerate(images):
        print(f"OCR de la page {i+1}...")
        text = pytesseract.image_to_string(image, lang='eng')
        ocr_text += f"\n\n# === Page {i+1} ===\n{text}"
    return ocr_text.strip()

# === LECTURE INTÉGRALE DU CODE EXISTANT ===
def read_all_code(path):
    with open(path, "r", encoding="utf-8", errors="ignore") as f:
        return f.read()

# === EXTRACTION DU CONTENU OCR ET CODE ===
results_text = extract_text_via_ocr(PDF_PATH)
code_content = read_all_code(CODE_PATH)

# === PRÉPARATION DU PROMPT ===
prompt = f"""
Voici un bout du code qui n'est pas complet : il y a peut-être du code en plus non divulgué de l'équipe. À l'aide du PDF complet, reconstruis le code dans son intégralité avec toutes les informations disponibles.

Le projet est situé ici :
setwd('/home/mrenzo/many-analysts-variations-revisit/')
data <- read.csv(file="data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

Important :
- Le CSV contient les valeurs `ratings` entre 0 et 1.
- Pour les modèles 1 et 2, il faut transformer ces valeurs pour qu'elles soient entre 1 et 5.
- Pour les modèles 3 et 4, tu peux conserver l’échelle de 0 à 1.

=== Texte OCR du PDF ===
{results_text}

=== Code existant ===
{code_content}
"""

# === SAUVEGARDE DU PROMPT UTILISÉ ===
with open("prompt_utilise.txt", "w", encoding="utf-8") as f:
    f.write(prompt)

# === ENVOI À L’API ===
headers = {
    "Authorization": f"Bearer {API_KEY}",
    "Content-Type": "application/json",
}

data = {
    "model": MODEL,
    "messages": [{"role": "user", "content": prompt}],
    "max_tokens": 2048
}

print("🚀 Envoi du prompt à l'API...")
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

    print(f"Code extrait et sauvegardé dans {output_filename}")
    print(f"Prompt sauvegardé dans prompt_utilise.txt")

except Exception as e:
    print("Erreur :", e)
    print("Réponse brute :", response.status_code, response.text)
