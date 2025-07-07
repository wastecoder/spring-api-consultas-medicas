package com.consultas.api_consultas.configs;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SwaggerConfig {

    @Bean
    public OpenAPI customOpenAPI() {
        final String schemeName = "basicAuth";

        return new OpenAPI()
                .info(new Info()
                        .title("API de Agendamentos Médicos")
                        .version("1.0.0")
                        .description("Documentação da API para gerenciamento de consultas médicas, incluindo cadastro de pacientes, médicos e agendamentos.")
                        .contact(new Contact()
                                .name("Pedro")
                                .email("pedromendes7182@gmail.com")
                                .url("https://github.com/wastecoder"))
                        .license(new License()
                                .name("Apache 2.0")
                                .url("https://www.apache.org/licenses/LICENSE-2.0.html"))
                )
                .addSecurityItem(new SecurityRequirement().addList(schemeName))
                .components(new Components()
                        .addSecuritySchemes(
                                schemeName,
                                new SecurityScheme()
                                        .name(schemeName)
                                        .type(SecurityScheme.Type.HTTP)
                                        .scheme("basic")
                        )
                );
    }

}
