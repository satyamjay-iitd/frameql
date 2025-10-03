use eframe::egui;

pub struct MyApp {
    query_text: String,
}

impl Default for MyApp {
    fn default() -> Self {
        Self {
            query_text: String::new(),
        }
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Left side panel
        egui::SidePanel::left("side_panel").show(ctx, |ui| {
            ui.vertical(|ui| {
                if ui.button("Import Video").clicked() {
                    // handle import
                }
                if ui.button("Select Video").clicked() {
                    // handle selection
                }

                ui.label("Query");
                ui.text_edit_multiline(&mut self.query_text);

                if ui.button("Visualize Video").clicked() {
                    // handle video visualization
                }
                if ui.button("Visualize Query").clicked() {
                    // handle query visualization
                }
            });
        });

        // Main panel for video display
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.centered_and_justified(|ui| {
                ui.label("Video"); // Replace with actual video texture
            });
        });
    }
}
