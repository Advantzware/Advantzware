using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class corr_miscsub : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        FormView_MiscSub.ChangeMode(FormViewMode.ReadOnly);
        MiscSub_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "Select";
                
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "corr_miscsub.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;


            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Label compname = (Label)Master.FindControl("lblComp");
            Label username = (Label)Master.FindControl("lblUser");
            Label labelname = (Label)Master.FindControl("lbl_page");
            compname.Text = PrmComp;
            username.Text = UserLogin.UserName;
            labelname.Text = "Corrugated";

            if (aUsers == "external")
            {
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        try
        {
            //Label type = (Label)FormView_Layout.FindControl("vTypeLabel");
            //Session["corr_type_val"] = type.Text;

            /*ImageButton miscsub = (ImageButton)Master.FindControl("Img_MiscSub");
            miscsub.ImageUrl = "~/Images/misc_sub_1.jpg";*/
            Image img_mov_col = (Image)Master.FindControl("Image5");
            img_mov_col.Visible = false;
        }
        catch { }
    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Label compname = (Label)Master.FindControl("lblComp");

            Label s1 = (Label)FormView_MiscSub.FindControl("vS1Label");
            TextBox b1 = (TextBox)FormView_MiscSub.FindControl("vB1TextBox");
            TextBox cost1 = (TextBox)FormView_MiscSub.FindControl("vCost1TextBox");
            TextBox matf1 = (TextBox)FormView_MiscSub.FindControl("vMatf1TextBox");
            TextBox labf1 = (TextBox)FormView_MiscSub.FindControl("vLabf1TextBox");
            TextBox matm1 = (TextBox)FormView_MiscSub.FindControl("vMatm1TextBox");
            TextBox labm1 = (TextBox)FormView_MiscSub.FindControl("vLabm1TextBox");
            DropDownList simon1 = (DropDownList)FormView_MiscSub.FindControl("vSimon1DropDownList");
            TextBox markup1 = (TextBox)FormView_MiscSub.FindControl("vMarkUp1TextBox");
            Label s2 = (Label)FormView_MiscSub.FindControl("vS2Label");
            TextBox b2 = (TextBox)FormView_MiscSub.FindControl("vB2TextBox");
            TextBox cost2 = (TextBox)FormView_MiscSub.FindControl("vCost2TextBox");
            TextBox matf2 = (TextBox)FormView_MiscSub.FindControl("vMatf2TextBox");
            TextBox labf2 = (TextBox)FormView_MiscSub.FindControl("vLabf2TextBox");
            TextBox matm2 = (TextBox)FormView_MiscSub.FindControl("vMatm2TextBox");
            TextBox labm2 = (TextBox)FormView_MiscSub.FindControl("vLabm2TextBox");
            DropDownList simon2 = (DropDownList)FormView_MiscSub.FindControl("vSimon2DropDownList");
            TextBox markup2 = (TextBox)FormView_MiscSub.FindControl("vMarkUp2TextBox");
            Label s3 = (Label)FormView_MiscSub.FindControl("vS3Label");
            TextBox b3 = (TextBox)FormView_MiscSub.FindControl("vB3TextBox");
            TextBox cost3 = (TextBox)FormView_MiscSub.FindControl("vCost3TextBox");
            TextBox matf3 = (TextBox)FormView_MiscSub.FindControl("vMatf3TextBox");
            TextBox labf3 = (TextBox)FormView_MiscSub.FindControl("vLabf3TextBox");
            TextBox matm3 = (TextBox)FormView_MiscSub.FindControl("vMatm3TextBox");
            TextBox labm3 = (TextBox)FormView_MiscSub.FindControl("vLabm3TextBox");
            DropDownList simon3 = (DropDownList)FormView_MiscSub.FindControl("vSimon3DropDownList");
            TextBox markup3 = (TextBox)FormView_MiscSub.FindControl("vMarkUp3TextBox");
            Label s4 = (Label)FormView_MiscSub.FindControl("vS4Label");
            TextBox b4 = (TextBox)FormView_MiscSub.FindControl("vB4TextBox");
            TextBox cost4 = (TextBox)FormView_MiscSub.FindControl("vCost4TextBox");
            TextBox matf4 = (TextBox)FormView_MiscSub.FindControl("vMatf4TextBox");
            TextBox labf4 = (TextBox)FormView_MiscSub.FindControl("vLabf4TextBox");
            TextBox matm4 = (TextBox)FormView_MiscSub.FindControl("vMatm4TextBox");
            TextBox labm4 = (TextBox)FormView_MiscSub.FindControl("vLabm4TextBox");
            DropDownList simon4 = (DropDownList)FormView_MiscSub.FindControl("vSimon4DropDownList");
            TextBox markup4 = (TextBox)FormView_MiscSub.FindControl("vMarkUp4TextBox");
            Label s5 = (Label)FormView_MiscSub.FindControl("vS5Label");
            TextBox b5 = (TextBox)FormView_MiscSub.FindControl("vB5TextBox");
            TextBox cost5 = (TextBox)FormView_MiscSub.FindControl("vCost5TextBox");
            TextBox matf5 = (TextBox)FormView_MiscSub.FindControl("vMatf5TextBox");
            TextBox labf5 = (TextBox)FormView_MiscSub.FindControl("vLabf5TextBox");
            TextBox matm5 = (TextBox)FormView_MiscSub.FindControl("vMatm5TextBox");
            TextBox labm5 = (TextBox)FormView_MiscSub.FindControl("vLabm5TextBox");
            DropDownList simon5 = (DropDownList)FormView_MiscSub.FindControl("vSimon5DropDownList");
            TextBox markup5 = (TextBox)FormView_MiscSub.FindControl("vMarkUp5TextBox");
            Label s6 = (Label)FormView_MiscSub.FindControl("vS6Label");
            TextBox b6 = (TextBox)FormView_MiscSub.FindControl("vB6TextBox");
            TextBox cost6 = (TextBox)FormView_MiscSub.FindControl("vCost6TextBox");
            TextBox matf6 = (TextBox)FormView_MiscSub.FindControl("vMatf6TextBox");
            TextBox labf6 = (TextBox)FormView_MiscSub.FindControl("vLabf6TextBox");
            TextBox matm6 = (TextBox)FormView_MiscSub.FindControl("vMatm6TextBox");
            TextBox labm6 = (TextBox)FormView_MiscSub.FindControl("vLabm6TextBox");
            DropDownList simon6 = (DropDownList)FormView_MiscSub.FindControl("vSimon6DropDownList");
            TextBox markup6 = (TextBox)FormView_MiscSub.FindControl("vMarkUp6TextBox");

            TextBox item1 = (TextBox)FormView_MiscSub.FindControl("vItem1TextBox");
            TextBox itemdesc1 = (TextBox)FormView_MiscSub.FindControl("vItemDscr1TextBox");
            TextBox qty1 = (TextBox)FormView_MiscSub.FindControl("vQty1TextBox");
            Label uom1 = (Label)FormView_MiscSub.FindControl("vUom1Label");
            TextBox item2 = (TextBox)FormView_MiscSub.FindControl("vItem2TextBox");
            TextBox itemdesc2 = (TextBox)FormView_MiscSub.FindControl("vItemDscr2TextBox");
            TextBox qty2 = (TextBox)FormView_MiscSub.FindControl("vQty2TextBox");
            Label uom2 = (Label)FormView_MiscSub.FindControl("vUom2Label");
            TextBox item3 = (TextBox)FormView_MiscSub.FindControl("vItem3TextBox");
            TextBox itemdesc3 = (TextBox)FormView_MiscSub.FindControl("vItemDscr3TextBox");
            TextBox qty3 = (TextBox)FormView_MiscSub.FindControl("vQty3TextBox");
            Label uom3 = (Label)FormView_MiscSub.FindControl("vUom3Label");
            TextBox item4 = (TextBox)FormView_MiscSub.FindControl("vItem4TextBox");
            TextBox itemdesc4 = (TextBox)FormView_MiscSub.FindControl("vItemDscr4TextBox");
            TextBox qty4 = (TextBox)FormView_MiscSub.FindControl("vQty4TextBox");
            Label uom4 = (Label)FormView_MiscSub.FindControl("vUom4Label");
            TextBox item5 = (TextBox)FormView_MiscSub.FindControl("vItem5TextBox");
            TextBox itemdesc5 = (TextBox)FormView_MiscSub.FindControl("vItemDscr5TextBox");
            TextBox qty5 = (TextBox)FormView_MiscSub.FindControl("vQty5TextBox");
            Label uom5 = (Label)FormView_MiscSub.FindControl("vUom5Label");
            TextBox item6 = (TextBox)FormView_MiscSub.FindControl("vItem6TextBox");
            TextBox itemdesc6 = (TextBox)FormView_MiscSub.FindControl("vItemDscr6TextBox");
            TextBox qty6 = (TextBox)FormView_MiscSub.FindControl("vQty6TextBox");
            Label uom6 = (Label)FormView_MiscSub.FindControl("vUom6Label");
            TextBox item7 = (TextBox)FormView_MiscSub.FindControl("vItem7TextBox");
            TextBox itemdesc7 = (TextBox)FormView_MiscSub.FindControl("vItemDscr7TextBox");
            TextBox qty7 = (TextBox)FormView_MiscSub.FindControl("vQty7TextBox");
            Label uom7 = (Label)FormView_MiscSub.FindControl("vUom7Label");
            TextBox item8 = (TextBox)FormView_MiscSub.FindControl("vItem8TextBox");
            TextBox itemdesc8 = (TextBox)FormView_MiscSub.FindControl("vItemDscr8TextBox");
            TextBox qty8 = (TextBox)FormView_MiscSub.FindControl("vQty8TextBox");
            Label uom8 = (Label)FormView_MiscSub.FindControl("vUom8Label");

            MiscSub_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "Update";
            MiscSub_ObjectDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            MiscSub_ObjectDataSource.SelectParameters["prmComp"].DefaultValue = compname.Text;
            //MiscSub_ObjectDataSource.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_corrugated_est"]);
            //MiscSub_ObjectDataSource.SelectParameters["prmForm"].DefaultValue = Convert.ToString(Session["order_corrugated_formno"]);
            MiscSub_ObjectDataSource.SelectParameters["prmS1"].DefaultValue = s1.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmB1"].DefaultValue = b1.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmCost1"].DefaultValue = cost1.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMatf1"].DefaultValue = matf1.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMatm1"].DefaultValue = matm1.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmLabf1"].DefaultValue = labf1.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmLabm1"].DefaultValue = labm1.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmSimon1"].DefaultValue = simon1.SelectedValue.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMarkup1"].DefaultValue = markup1.Text.Trim();

            MiscSub_ObjectDataSource.SelectParameters["prmS2"].DefaultValue = s2.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmB2"].DefaultValue = b2.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmCost2"].DefaultValue = cost2.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMatf2"].DefaultValue = matf2.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMatm2"].DefaultValue = matm2.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmLabf2"].DefaultValue = labf2.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmLabm2"].DefaultValue = labm2.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmSimon2"].DefaultValue = simon2.SelectedValue.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMarkup2"].DefaultValue = markup2.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmS3"].DefaultValue = s3.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmB3"].DefaultValue = b3.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmCost3"].DefaultValue = cost3.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMatf3"].DefaultValue = matf3.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMatm3"].DefaultValue = matm3.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmLabf3"].DefaultValue = labf3.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmLabm3"].DefaultValue = labm3.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmSimon3"].DefaultValue = simon3.SelectedValue.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMarkup3"].DefaultValue = markup3.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmS4"].DefaultValue = s4.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmB4"].DefaultValue = b4.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmCost4"].DefaultValue = cost4.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMatf4"].DefaultValue = matf4.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMatm4"].DefaultValue = matm4.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmLabf4"].DefaultValue = labf4.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmLabm4"].DefaultValue = labm4.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmSimon4"].DefaultValue = simon4.SelectedValue.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMarkup4"].DefaultValue = markup4.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmS5"].DefaultValue = s5.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmB5"].DefaultValue = b5.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmCost5"].DefaultValue = cost5.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMatf5"].DefaultValue = matf5.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMatm5"].DefaultValue = matm5.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmLabf5"].DefaultValue = labf5.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmLabm5"].DefaultValue = labm5.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmSimon5"].DefaultValue = simon5.SelectedValue.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMarkup5"].DefaultValue = markup5.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmS6"].DefaultValue = s6.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmB6"].DefaultValue = b6.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmCost6"].DefaultValue = cost6.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMatf6"].DefaultValue = matf6.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMatm6"].DefaultValue = matm6.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmLabf6"].DefaultValue = labf6.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmLabm6"].DefaultValue = labm6.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmSimon6"].DefaultValue = simon6.SelectedValue.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmMarkup6"].DefaultValue = markup6.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItem1"].DefaultValue = item1.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItemDesc1"].DefaultValue = itemdesc1.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmQty1"].DefaultValue = qty1.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItem2"].DefaultValue = item2.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItemDesc2"].DefaultValue = itemdesc2.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmQty2"].DefaultValue = qty2.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItem3"].DefaultValue = item3.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItemDesc3"].DefaultValue = itemdesc3.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmQty3"].DefaultValue = qty3.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItem4"].DefaultValue = item4.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItemDesc4"].DefaultValue = itemdesc4.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmQty4"].DefaultValue = qty4.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItem5"].DefaultValue = item5.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItemDesc5"].DefaultValue = itemdesc5.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmQty5"].DefaultValue = qty5.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItem6"].DefaultValue = item6.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItemDesc6"].DefaultValue = itemdesc6.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmQty6"].DefaultValue = qty6.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItem7"].DefaultValue = item7.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItemDesc7"].DefaultValue = itemdesc7.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmQty7"].DefaultValue = qty7.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItem8"].DefaultValue = item8.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmItemDesc8"].DefaultValue = itemdesc8.Text.Trim();
            MiscSub_ObjectDataSource.SelectParameters["prmQty8"].DefaultValue = qty8.Text.Trim();
        }
        catch { }
    }
    
}
