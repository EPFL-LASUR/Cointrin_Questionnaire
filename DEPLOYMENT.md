# Deployment Guide

This guide explains how to deploy the Cointrin Questionnaire for public access.

## Local Network Deployment

### Option 1: Run on Local Network
1. Find your computer's IP address:
   - Windows: `ipconfig`
   - Mac/Linux: `ifconfig` or `ip addr`

2. Run the app with host parameter:
```r
shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
```

3. Share the URL with participants on the same network:
   - `http://YOUR_IP_ADDRESS:3838`

## Cloud Deployment

### Option 1: shinyapps.io (Easiest)

shinyapps.io is RStudio's hosting service with a free tier.

1. **Create an account** at [shinyapps.io](https://www.shinyapps.io/)

2. **Install rsconnect package:**
```r
install.packages("rsconnect")
```

3. **Configure your account:**
```r
library(rsconnect)
rsconnect::setAccountInfo(
  name = "your-account-name",
  token = "your-token",
  secret = "your-secret"
)
```
(Get token and secret from your shinyapps.io account settings)

4. **Deploy the app:**
```r
rsconnect::deployApp()
```

5. **Your app will be available at:**
   - `https://your-account-name.shinyapps.io/Cointrin_Questionnaire/`

**Free tier limits:**
- 5 applications
- 25 active hours per month
- 1GB memory per application

### Option 2: Shiny Server (Self-hosted)

For more control and unlimited usage.

1. **Install Shiny Server** on a Linux server:
   - Follow instructions at [Shiny Server Guide](https://rstudio.com/products/shiny/download-server/)

2. **Copy app files** to `/srv/shiny-server/cointrin/`

3. **Access your app at:**
   - `http://your-server-ip:3838/cointrin/`

4. **Configure** `/etc/shiny-server/shiny-server.conf` as needed

### Option 3: Docker Deployment

1. **Create Dockerfile:**
```dockerfile
FROM rocker/shiny:latest

# Install required packages
RUN R -e "install.packages(c('shiny', 'shinyjs', 'ggplot2'), repos='https://cran.rstudio.com/')"

# Copy app files
COPY . /srv/shiny-server/cointrin/

# Expose port
EXPOSE 3838

# Run app
CMD ["/usr/bin/shiny-server"]
```

2. **Build and run:**
```bash
docker build -t cointrin-questionnaire .
docker run -p 3838:3838 cointrin-questionnaire
```

### Option 4: RStudio Connect (Enterprise)

For organizations with RStudio Connect:

1. Open the project in RStudio
2. Click "Publish" button
3. Select RStudio Connect
4. Follow the deployment wizard

## Data Management

### Important Considerations

1. **Data Privacy:**
   - Ensure compliance with GDPR and other privacy regulations
   - Store data securely
   - Consider anonymization strategies

2. **Backup Strategy:**
   - Regularly backup `questionnaire_responses.csv`
   - Set up automated backups if deployed to server
   - Consider database integration for large-scale surveys

3. **Access Control:**
   - Limit access to response data
   - Use authentication if needed (shinymanager package)
   - Consider IP restrictions for sensitive surveys

### Database Integration (Advanced)

For production use, consider storing responses in a database:

```r
# Example with SQLite
library(DBI)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "responses.db")

# In server function, replace CSV writing with:
dbWriteTable(con, "responses", responses, append = TRUE)
```

## Monitoring

### Track Usage
- shinyapps.io provides built-in analytics
- For self-hosted: use server logs
- Consider Google Analytics integration

### Example Analytics Integration:
```r
# Add to ui section
tags$head(
  tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=GA_TRACKING_ID"),
  tags$script(HTML("
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments);}
    gtag('js', new Date());
    gtag('config', 'GA_TRACKING_ID');
  "))
)
```

## Scaling Considerations

### For High-Traffic Surveys:
1. Use a dedicated server with sufficient resources
2. Implement load balancing for multiple instances
3. Use a proper database instead of CSV files
4. Set up caching strategies
5. Monitor server performance

## Security Best Practices

1. **Use HTTPS** (SSL/TLS certificates)
2. **Sanitize user inputs** (Shiny does this by default)
3. **Regular updates** of R and packages
4. **Firewall configuration** to limit access
5. **Rate limiting** to prevent abuse

## Troubleshooting Deployment

### Common Issues:

**"Application failed to start"**
- Check R version compatibility
- Verify all packages are installed
- Review server logs for errors

**"Disconnected from server"**
- Increase timeout settings
- Check memory limits
- Verify network connectivity

**"Package not found"**
- Ensure all packages are installed on server
- Check CRAN mirror accessibility
- Install system dependencies if needed

## Support

For deployment issues:
1. Check [Shiny documentation](https://shiny.rstudio.com/)
2. Visit [RStudio Community](https://community.rstudio.com/)
3. Open an issue in this repository

---

**Note:** Choose the deployment method that best fits your technical expertise and requirements. For quick testing, shinyapps.io free tier is recommended. For production use with sensitive data, consider self-hosted options.
