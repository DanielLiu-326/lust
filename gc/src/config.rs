pub struct GcConfig {
    pub min_gc: usize,
    pub alloc_factor: usize,
}

impl Default for GcConfig{
    #[inline(always)]
    fn default() -> Self {
        Self{
            min_gc: 0,
            alloc_factor: 200,
        }
    }
}