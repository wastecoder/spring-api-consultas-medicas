package com.consultas.api_consultas.dtos;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import java.util.List;

public record PageResponse<T>(

        List<T> content,
        int page,
        int size,
        long totalElements,
        int totalPages,
        boolean first,
        boolean last,
        boolean hasNext,
        boolean hasPrevious

) {
    public static <T> PageResponse<T> from(Page<T> page) {
        return new PageResponse<>(
                page.getContent(),
                page.getNumber(),
                page.getSize(),
                page.getTotalElements(),
                page.getTotalPages(),
                page.isFirst(),
                page.isLast(),
                page.hasNext(),
                page.hasPrevious()
        );
    }

    public static <T> PageResponse<T> fromList(List<T> source, Pageable pageable) {
        int total = source.size();
        int inicio = Math.min((int) pageable.getOffset(), total);
        int fim = Math.min(inicio + pageable.getPageSize(), total);
        return from(new PageImpl<>(source.subList(inicio, fim), pageable, total));
    }
}
