# Explore Theme

Explore uses `SemanticUI` as the css framework. `SemanticUI` supports themes and comes with a few predefined ones, whose settings can be further overridden for customization.

The theme used in `explore` is based on `default`, existing at `common/src/main/webapp/suithemes/default` overridden with values at `common/src/main/webapp/theme`.

The original themes are made in less but that limits the option to dynamically change the theme e.g.
from dark/light. Thus the `explore` theme uses css-variables instead using the approach described
[here](https://css-tricks.com/a-complete-guide-to-dark-mode-on-the-web/)

Note that only colors are replaced by css variables and other settings like padding etc remain as less variables.

The `light` theme is taken from the default theme while `dark` is based on a SUI theme called [slate](https://github.com/semantic-ui-forest/forest-themes/tree/master/src/themes/bootswatch/v4/slate).
Since `slate` isn't being maintained it has become the "explore" theme.

# Naming conventions

Naming variables in a way that is more or less understandable and matching `SemanticUI` is key.

The convention taken is to name variables using a prefix that corresponds to the section in `SUI` and the original name of the less variable. e.g.

In the default _theme/menu.variables_ file there is:

```less
@hoverItemBackground: var(--menu-hover-item-background);
```

This is replaced by a variable :

- Dark theme:

```css
--menu-hover-item-background: var(--color-background-light-10);
```

- Light theme:

```css
--menu-hover-item-background: @subtleTransparentBlack;
```

Note that in the example above on the light theme we use the original definition, while
on the dark theme we are further referring another color in a css variable.

Furthermore groups or variables are grouped using less mixins like

```less
.menu-dark() {
  --menu-hover-item-background: var(--color-background-light-10);
}
```

We use a mixin as this needs to be referred twice.
For each mixin we have a light and a dark version

# Calculated colors

The approach of further referring colors in css variables is used because, in general, dark themes use variations on lightness of a base color.

This was originally handled in less using e.g.:

```less
@hoverItemBackground: lighten(@pageBackground, 10%);
```

However this is statically compiled while we need to dynamically change the colors in a css variable.
Using the approach in [this article](https://blog.jim-nielsen.com/2020/sass-color-functions-in-css/) colors are decomposed onto there hsl/hsla components and shades can be calculated using css calc commands, e.g.:

```css
  --color-background-base-h: 0;
  --color-background-base-s: 0%;
  --color-background-base-l: 7.06%;
  --color-background-base-hsl: var(--color-background-base-h), var(--color-background-base-s),
    var(--color-background-base-l);
  --color-background: hsl(var(--color-background-base-hsl));

  --color-background-light-10: hsl(
    var(--color-background-base-h),
    var(--color-background-base-s),
    calc(var(--color-background-base-l) + 10%)
```

# References

As mentioned the theme is mostly based on `slate` but it has been further refined using input from:

[ClubHouse UI](https://app.clubhouse.io)
[Guide to dark mode](https://css-tricks.com/a-complete-guide-to-dark-mode-on-the-web/)
[Material UI Dark Theme](https://material.io/design/color/dark-theme.html#ui-application)

# Linting

[Stylelint](https://stylelint.io/) has been setup to check for common errors on the css/less/sass files.

A tricky part of using `stylelint` is the way variables are checked.
A small sed script can take the variables from either the dark or light theme and export them as a `vars.css` file by removing the lines that don't define variables.
This file in turns is used by `stylelint` to check that all variables required are present.
To run the linters locally you can do either:
`npm run lint-dark`
`npm run lint-light`

Note the light version has false positives as the variables used come from less and not all can be expanded by `stylelint`
