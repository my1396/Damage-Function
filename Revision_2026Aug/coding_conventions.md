**Coding conventions:**

- Separate the code into sections with a header like this:
  
  ```r
  ## ========================================================================== ##
  ## 1. Fan chart: point estimate + bootstrap density bands ----------------------
  ## ========================================================================== ##
  ```

  - Separator should start and end with `##`, padding with `=` to make it 80 characters long.
  - Section headers should be numbered sequentially and padded with dashes to the right to make them all the same length (80 characters).
  - Note the spaces before and after the section title, and in the separator lines.

**Plots:**

- Find my custom ggplot theme in `./_fig_theme.R`. Use it for all plots. 
- For figure backround (except for maps), always use "white". I think it is the default. Don't change it to some other color.