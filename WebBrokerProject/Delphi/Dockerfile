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

RUN chmod +x /app/WebStencilsDemo

# Switch to non-root user
USER appuser

# Environment variables
ENV APP_LOG_PATH=/app/logs/app.log \
    APP_RESOURCES_PATH=/app/resources \
    DOCKER_CONTAINER=1 \
    DEMO_MODE=true \
    DEMO_RESET_INTERVAL=900

EXPOSE 8080

VOLUME ["/app/logs", "/app/data"]

# Health check for web service (using bash built-in /dev/tcp - no dependencies needed)
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
    CMD bash -c 'exec 3<>/dev/tcp/localhost/8080 && echo -e "GET /health HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" >&3 && cat <&3 | grep -q "healthy" || exit 1'

CMD ["/app/WebStencilsDemo"]