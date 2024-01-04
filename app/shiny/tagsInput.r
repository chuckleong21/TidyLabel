tagsInput <- function(
    inputId, label, value = "",
    max = NULL, width = NULL, placeholder = NULL) {
    # input tag
    input <- tags$input(
        id = inputId,
        type = "text",
        class = "form-control input-text input-tag",
        value = value,
        placeholder = placeholder,
        if(!is.null(max)) {
        },

        # JS depedency
        htmltools::htmlDependency(
            name = "tagsInput",
            version = "0.0.1",
            src = c(file = normalizePath("./www")),
            script = "tags.js",
            stylesheet = "tags.css"
        )
    )
    
    if(!is.null(max)) {
      input <- htmltools::tagAppendAttributes(input, `data-max` = max)
    }
    
    # details and remove button
    details <- div(
        class = "details",
        p(paste0(max, "tags are remaining")),
        tags$button(class = "btn btn-default action-button shiny-bound-input", 
                    "Remove All")
    )

    tagList(
        div(
            class = "form-group shiny-container tagsInput",
            style = if (!is.null(width)) {
                paste0("width:", validateCssUnit(width), ";")
            },
            shiny:::shinyInputLabel(inputId, label = label),
            tags$ul(input),
            details
        )
    )
}

tagsInput(inputId = "tags", label = "Tags", max = 4)

