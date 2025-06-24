# Team 2 – Data Preparation and Analysis

First of setwd is the path to the repo so you should change it for your sitatuation

## Script: `team2.R`

The `team2.R` script prepares the raw dataset and performs the statistical analysis as described in Team 2’s report.

---

### Main Steps

#### 1. **Load packages and data**
- Loads the dataset `CrowdstormingDataJuly1st.csv`.
- Parses birth dates and converts necessary variables to numeric.
- Computes `age` and `age²`, as well as `height²` and `weight²`.
- Averages the two skin tone ratings: `rateravg = (rater1 + rater2)/2`.

#### 2. **Construct expanded dataset**
- Assigns a unique identifier to each player (`dyadnum`).
- Aggregates the total number of yellow, red, and yellow-red cards per player.
- Expands the dataset by the number of games (`uncount(games)`) to create one row per match.
- Distributes cards across the expanded rows (one card per match occurrence).

#### 3. **Recode card variables**
- Binary indicators for:
  - `yellowCards`
  - `yellowReds` (second yellow = red)
  - `redCards` (direct red)
- Composite indicators:
  - `anyred`: redCards OR yellowReds
  - `anycard`: any type of card

#### 4. **Encode categorical variables**
- Converts the following into factors:
  - `club` → `clubnum`
  - `position` → `positionnum`
  - `leagueCountry` → `leaguecountrynum`

#### 5. **Add implicit and explicit bias scores**
- Computes standardized (z-score) average IAT and Exp scores per referee country.
- Joins these scores back to the main dataset by `refCountry`.

---

### Statistical Models

#### Q1 – Effect of skin tone on cards

- **Linear Probability Models (LPMs)**:
  - Basic: `redCards ~ rateravg`
  - With controls: adds `height`, `weight`, `age` and their squares
  - With fixed effects: includes `leaguecountrynum`, `positionnum`, `clubnum`
- **Logistic regression**: with same variables
- **Non-linear model**: categorical `rater1` as a factor (levels 1 to 5)

#### Q1 – Alternate sanctions
- Models replicated for:
  - `anyred`: any red card (direct or 2nd yellow)
  - `yellowCards`: yellow card only

#### Q2 – Impact of implicit / explicit bias

- Creates `dark` dummy: `1` if `rater1 > 2/5`, else `0`.
- Separate regressions for light-skinned and dark-skinned players:
  - `redCards ~ zmeaniat`
  - `redCards ~ zmeanexp`

#### Marginal effects
- Computes marginal slope of `rateravg` using `avg_slopes()` on the logit model.

---

### Output

- All models are stored in a list.
- Model results are printed using `modelsummary()` with:
  - Standard errors
  - Significance stars
  - Fit statistics suppressed (`IC`, `LogLik`, etc.)

---

### Note

Results differ from those reported in the original PDF due to lack of transparency or missing details in the team’s original documentation.
