#### Using Bard in R ####
library(openai)

Sys.setenv(
  OPENAI_API_KEY = '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')

create_completion(
  model = "ada",
  prompt = "Generate a question and an answer"
)

create_image("An astronaut riding a horse in a photorealistic style")

list