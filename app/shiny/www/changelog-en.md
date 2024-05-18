---
output: html_document
---

<head>
  <script src="https://kit.fontawesome.com/f817ba0cef.js" crossorigin="anonymous"></script>
</head>


## Changelog

Only works on Windows since >=0.7.0 for dependencies of MS Applications and Java

### 1.2.0

- <i class="fa-regular fa-circle" style="color:steelblue;"></i> Feature: Check labels across container lists
- <i class="fa-regular fa-circle" style="color:steelblue;"></i> App Settings：add switch for `Check labels`

### 1.1.9.9000

- <i class="fa-regular fa-circle" style="color:steelblue;"></i> Add methods for container lists' standardization
- <i class="fa-regular fa-circle" style="color:steelblue;"></i> Improve `App Settings` UI

### 1.1.0 

- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> Add tidy method for tax version `2024-05`，retain previous tidy method
- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> Add `App Settings` tab
  - Move `Language` settings
  - Add tax version selection
- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> Update `tabulapdf` API
- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> Update internationalization

### 1.0.1

- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> Minor fixes

### 1.0.0

- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> Add English translation

### 0.9.0

- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> **TidyTax** now shows tax codes and their corresponding taxes
- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> Adds filters by summary and tax codes to **TidyTax**
- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> **TidyLabel** now moves label pictures from excel to word
- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> **TidyLabel** now has a `preview` option
- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> UI improvement
-   <i class="fas fa-question" style="color:red;"></i> Adds file caching

### 0.8.1

- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> Eliminates repetitive calculations in **TidyTax**
- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> Russian headers used in tax table in **TidyTax**

### 0.8.0

- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> **TidyTax** now deals with PDFs
- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> **TidyTax** abandons XLS\(X\) method
- <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> **TidyTax** now can rename headers

### 0.7.0

-   <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> **TIdyLabel** now can apply formats in a batch and output to a word document

### 0.6.0

-   <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> Encapsulates data wrangling methods into a `Shiny` web app

### 0.1.0

-   <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> Adds `TidyTax` data wrangling method
-   <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> Adds `TidyLabel` data wrangling method
