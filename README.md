<div align="center">
  <a href="https://github.com/Felioxx/Mobile-ity-Scope">
    <img src="https://github.com/Felioxx/Mobile-ity-Scope/blob/main/images/MobilityScope_logo.png?raw=true" alt="Logo" width="20%" height="20%">
  </a>
<h3 align="center">Mobility Scope</h3> 
 Explore the Movement of Londoners in 2020!
  <p align="center">
        <br />
   made <a href="https://www.uni-muenster.de/Geoinformatics/">@ifgi - UNI MUENSTER</a> 🌍
    <br />
    <a href="https://github.com/Felioxx//Mobile-ity-Scope/tree/main/App"><strong>Explore the docs »</strong></a>
  </p>
</div>
<p align="center">
-- 🇬🇧 ♔ - ⋆ ☂︎ ⋆ - ♔ 🇬🇧 --
     
</p>
<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li><a href="#about-the-project">About The Project</a></li>
    <li><a href="#the-analysis-tool">The Analysis Tool</a></li>
    <li><a href="#built-with">Built With</a></li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>

<!-- ABOUT THE PROJECT -->

## About The Project

This project was done during the winter term 2024/25 at ifgi Münster as part of the Analysis of Human Movement Data Course of the Master in Geoinformatics.

In this project an interactive Shiny App in R for the analysis of the movement of Londoners during the year 2020 was implemented. The App includes a map and two interactive plots which focus on the Movement around museums and shops in Greater London.

These components, which will be explained in the following:

## The Analysis Tool

<div class="image">
  <img align=left width="50%" src="https://github.com/Felioxx/Mobile-ity-Scope/blob/main/images/Map_Screenshot.png">
</div>

### The Mobility-Map

The Mobility Map displays the movement data in at least 100m x 100m large tiles. The color indicates the activity. Additionally the user can set some parameters before calculating the map. But first they have to decide wether to calculate the activity for one day or for the relative change between two days.
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
The parameters are:

- The date
- Optional: a date for comparison
- The districts of Greater London
- The tile size
  - 100m²
  - 200m²
  - 400m²
  - 800m²

The map also displays the locations of 197 museums and 80 shoopping centres.

---

<div class="image">
  <img align=left width="50%" src="https://github.com/Felioxx/Mobile-ity-Scope/blob/main/images/Plot_Screenshot.png">
</div>

### The Interactive Mobility-Plot

The Mobility Plot shows the activity around the single musuems and shopping centres for the whole year 2020. The user can select several PoIs and see the development of the measured activity for the whole year but they can also zoom in and analyse specific dates.
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />

---

<div class="image">
  <img align=left width="50%" src="https://github.com/Felioxx/Mobile-ity-Scope/blob/main/images/Aggregated_Plot_Screenshot.png">
</div>

### The Aggregated Mobility-Plot

The Aggregated Mobility-plot visualizes the difference between the activity around museums and shopping centres

<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />

<p align="right">(<a href="https://github.com/Felioxx/Mobile-ity-Scope/tree/main?tab=readme-ov-file#mobility-scope">back to top</a>)</p>

---

### Built With

- [![python][python.com]][python-url]
- [![StackOverflow][StackOverflow.com]][StackOverflow-url]
- ![rstudio][rstudio-url]

<p align="right">(<a href="https://github.com/Felioxx/Mobile-ity-Scope/tree/main?tab=readme-ov-file#mobility-scope">back to top</a>)</p>

<!-- GETTING STARTED -->

## Getting Started

To start the shiny app the code in `first_steps_R/shinyApp.R` must be executed. Before that the required libaries must be installed and the working directory must be set to the folder of the repository. The following data is needed, which is not included in this repository:

- pre_processed_movement.parquet
- distinct_LONLAT.parquet

Also the part in row number 232 til 243 can be skipped by loading in the object `first_steps_R/movement_data.RData`. The code for that is already in the script.

<p align="right">(<a href="https://github.com/Felioxx/Mobile-ity-Scope/tree/main?tab=readme-ov-file#mobility-scope">back to top</a>)</p>

<!-- LICENSE -->

## License

Copyright (c) 2025 Spacey GmbH

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="https://github.com/Felioxx/Mobile-ity-Scope/tree/main?tab=readme-ov-file#mobility-scope">back to top</a>)</p>

<!-- CONTACT -->

## Contact

Eva Langstein - elangste@uni-muenster.de

Anne Staskiewicz - anne.staskiewicz@uni-muenster.de

Felix Disselkamp - fdisselk@uni-muenster.de

##### Project Link: [https://github.com/Felioxx/Mobile-ity-Scope](https://github.com/Felioxx/Mobile-ity-Scope)

<p align="right">(<a href="https://github.com/Felioxx/Mobile-ity-Scope/tree/main?tab=readme-ov-file#mobility-scope">back to top</a>)</p>

<!-- Improved compatibility of back to top link: See: https://github.com/othneildrew/Best-README-Template/pull/73 -->

<a name="readme-top"></a>

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->

[contributors-shield]: https://img.shields.io/github/contributors/github_username/repo_name.svg?style=for-the-badge
[contributors-url]: https://github.com/github_username/repo_name/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/github_username/repo_name.svg?style=for-the-badge
[forks-url]: https://github.com/github_username/repo_name/network/members
[stars-shield]: https://img.shields.io/github/stars/github_username/repo_name.svg?style=for-the-badge
[stars-url]: https://github.com/github_username/repo_name/stargazers
[issues-shield]: https://img.shields.io/github/issues/github_username/repo_name.svg?style=for-the-badge
[issues-url]: https://github.com/github_username/repo_name/issues
[license-shield]: https://img.shields.io/github/license/github_username/repo_name.svg?style=for-the-badge
[license-url]: https://github.com/github_username/repo_name/blob/master/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/linkedin_username
[product-screenshot]: images/screenshot.png
[Next.js]: https://img.shields.io/badge/next.js-000000?style=for-the-badge&logo=nextdotjs&logoColor=white
[Next-url]: https://nextjs.org/
[React.js]: https://img.shields.io/badge/React-20232A?style=for-the-badge&logo=react&logoColor=61DAFB
[React-url]: https://reactjs.org/
[Vue.js]: https://img.shields.io/badge/Vue.js-35495E?style=for-the-badge&logo=vuedotjs&logoColor=4FC08D
[Vue-url]: https://vuejs.org/
[Angular.io]: https://img.shields.io/badge/Angular-DD0031?style=for-the-badge&logo=angular&logoColor=white
[Angular-url]: https://angular.io/
[Svelte.dev]: https://img.shields.io/badge/Svelte-4A4A55?style=for-the-badge&logo=svelte&logoColor=FF3E00
[Svelte-url]: https://svelte.dev/
[Laravel.com]: https://img.shields.io/badge/Laravel-FF2D20?style=for-the-badge&logo=laravel&logoColor=white
[Laravel-url]: https://laravel.com
[Bootstrap.com]: https://img.shields.io/badge/Bootstrap-563D7C?style=for-the-badge&logo=bootstrap&logoColor=white
[Bootstrap-url]: https://getbootstrap.com
[JQuery.com]: https://img.shields.io/badge/jQuery-0769AD?style=for-the-badge&logo=jquery&logoColor=white
[JQuery-url]: https://jquery.com
[JavaScript.com]: https://img.shields.io/badge/javascript-%23323330.svg?style=for-the-badge&logo=javascript&logoColor=%23F7DF1E
[JavaScript-url]: https://www.javascript.com/
[StackOverflow-url]: https://stackoverflow.com/
[StackOverflow.com]: https://img.shields.io/badge/-Stackoverflow-FE7A16?style=for-the-badge&logo=stack-overflow&logoColor=white
[openAi-url]: https://openai.com/
[openAi.com]: https://img.shields.io/badge/-OpenAI%20API-eee?style=for-the-badge&logo=openai&logoColor=412991

[nodeJS-url]: [https://pixijs.com/](https://nodejs.org/en)
[nodeJS.com]: https://img.shields.io/badge/node.js-6DA55F?style=for-the-badge&logo=node.js&logoColor=white
[neo4j.com]: https://img.shields.io/badge/neo4j-4581C3?style=for-the-badge&logo=neo4j&logoColor=white
[neo4j-url]: https://neo4j.com/
[python.com]: https://img.shields.io/badge/python-3670A0?style=for-the-badge&logo=python&logoColor=ffdd54
[python-url]: https://www.python.org/
[rstudio-url]: https://img.shields.io/badge/RStudio-4285F4?style=for-the-badge&logo=rstudio&logoColor=white
