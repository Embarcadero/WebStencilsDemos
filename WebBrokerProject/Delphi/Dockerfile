FROM gcr.io/distroless/cc

# Set a working directory
WORKDIR /app

# Copy the compiled Delphi application
COPY Linux64/Docker/WebStencilsDemo /app/WebStencilsDemo

# Copy required resources
COPY resources /app/resources

# Create backup directory and copy initial database
COPY resources/data/database.sqlite3 /app/backup/database.sqlite3

# Expose the port the application runs on
EXPOSE 8080

# Set the stop signal
STOPSIGNAL SIGTERM

# Create volumes for logs and database
VOLUME ["/app/logs", "/app/data"]

# Set environment variables
ENV APP_LOG_PATH=/app/logs/app.log
ENV APP_DB_PATH=/app/data/database.sqlite3
ENV APP_RESOURCES_PATH=/app/resources
ENV DOCKER_CONTAINER=1

# Command to run the application
CMD ["/app/WebStencilsDemo"] 