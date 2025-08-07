
sudo apt update -y
sudo apt install tree -y

# Clônage du projet
cd ~/work/
git clone https://github.com/julienjamme/ckm_risk_utility_experiments.git
cd ~/work/ckm_risk_utility_experiments/

# Récupération du nom de code Ubuntu (ex: focal, jammy)
if [ -f /etc/os-release ]; then
    . /etc/os-release
    UBUNTUNAME="$VERSION_CODENAME"
else
    UBUNTUNAME=$(lsb_release -cs 2>/dev/null || echo "unknown")
fi

# Construction de l'URL du dépôt
REPOSITR="https://packagemanager.posit.co/cran/__linux__/${UBUNTUNAME}/latest/"

# Installation des packages R nécessaires
Rscript -e "install.packages('remotes', repos='${REPOSITR}')"
Rscript -e "remotes::install_deps(repos='${REPOSITR}', upgrade="always")"
Rscript -e "remotes::install_github('InseeFrLab/ckm', dependencies = TRUE, build_vignettes = FALSE, upgrade='never')"
