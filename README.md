# budget

## Setup
```sh
# Build postgres image with budget user and DB
docker build -t budgetdb .

# Run postgres in the background, exposing port 5432
docker run -d -p5432:5432 dbbudget
```