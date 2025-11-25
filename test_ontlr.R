# TESTE MÍNIMO - test_minimal.R
cat("Teste mínimo do ontlr...\n")

# Carregar dependências diretamente
library(httr)
library(jsonlite)

# Tentar carregar ontlr
tryCatch({
  devtools::load_all()
  cat("ontlr carregada\n")
  
  # Teste básico
  meta <- get_metadados()
  cat("get_metadados() funcionou -", length(meta), "elementos\n")
  
  colunas <- get_metadados_filterable_columns() 
  cat("get_metadados_filterable_columns() funcionou -", nrow(colunas), "colunas\n")
  
}, error = function(e) {
  cat("Erro:", e$message, "\n")
})