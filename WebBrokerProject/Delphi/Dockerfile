FROM debian:13-slim

# Install security updates and clean up in single layer
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Create non-root user
RUN useradd -m -u 1001 appuser && \
    mkdir -p /app/logs /app/data /app/backup && \
    chown -R appuser:appuser /app

WORKDIR /app

# Copy application and set permissions
COPY --chown=appuser:appuser Linux64/Release/WebStencilsDemo /app/WebStencilsDemo
COPY --chown=appuser:appuser ../resources /app/resources
COPY --chown=appuser:appuser ../resources/data/database.sqlite3 /app/backup/database.sqlite3

RUN chmod +x /app/WebStencilsDemo

# Switch to non-root user
USER appuser

# Environment variables
ENV APP_LOG_PATH=/app/logs/app.log \
    APP_DB_PATH=/app/data/database.sqlite3 \
    APP_RESOURCES_PATH=/app/resources \
    DOCKER_CONTAINER=1

EXPOSE 8080

VOLUME ["/app/logs", "/app/data"]

# Health check for web service
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD ["/usr/bin/curl", "-f", "http://localhost:8080/", "||", "exit", "1"]

CMD ["/app/WebStencilsDemo"]