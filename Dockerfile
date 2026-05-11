# Etapa de build: compila o jar com Maven Wrapper
FROM eclipse-temurin:17-jdk AS build
WORKDIR /workspace

COPY .mvn .mvn
COPY mvnw pom.xml ./
RUN chmod +x mvnw && ./mvnw -B -ntp dependency:go-offline

COPY src src
RUN ./mvnw -B -ntp clean package -DskipTests

# Etapa de runtime: somente JRE + jar final
FROM eclipse-temurin:17-jre
WORKDIR /app

COPY --from=build /workspace/target/api-consultas-*.jar app.jar

EXPOSE 8080
ENTRYPOINT ["java","-jar","/app/app.jar"]
