chickens_on_fire = 12
vertical_names = 1:chickens_on_fire
horizontal_names = "Are you French? Because Eiffel for you"
for (i in vertical_names) {
  horizontal_names[i] = 2008 + i
}
triangle_test = matrix(
  data = NA,
  nrow = chickens_on_fire,
  ncol = chickens_on_fire,
  byrow = FALSE,
  dimnames = NULL
)
horizontal_names = as.numeric(horizontal_names)
rownames(triangle_test) = horizontal_names
colnames(triangle_test) = vertical_names
triangle_test