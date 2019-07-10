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

public partial class corr_bom : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        try
        {
            UserClass UserLogin = (UserClass)Session["User"];
            FormView1.ChangeMode(FormViewMode.Edit);
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        }
        catch
        {
            Response.Write("Session Expired! Please Relogin");
        }
    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        try
        {
            TextBox item1 = (TextBox)FormView1.FindControl("vBomItem1TextBox");
            TextBox item2 = (TextBox)FormView1.FindControl("vBomItem2TextBox");
            TextBox item3 = (TextBox)FormView1.FindControl("vBomItem3TextBox");
            TextBox item4 = (TextBox)FormView1.FindControl("vBomItem4TextBox");
            TextBox item5 = (TextBox)FormView1.FindControl("vBomItem5TextBox");
            TextBox item6 = (TextBox)FormView1.FindControl("vBomItem6TextBox");
            TextBox item7 = (TextBox)FormView1.FindControl("vBomItem7TextBox");
            TextBox item8 = (TextBox)FormView1.FindControl("vBomItem8TextBox");
            TextBox lamcode = (TextBox)FormView1.FindControl("vBomItem9TextBox");
            TextBox adhesive = (TextBox)FormView1.FindControl("vBomItem10TextBox");
            //TextBox itemdesc1 = (TextBox)FormView1.FindControl("vBomItemDscr1TextBox");
            //TextBox itemdesc2 = (TextBox)FormView1.FindControl("vBomItemDscr2TextBox");
            //TextBox itemdesc3 = (TextBox)FormView1.FindControl("vBomItemDscr3TextBox");
            //TextBox itemdesc4 = (TextBox)FormView1.FindControl("vBomItemDscr4TextBox");
            //TextBox itemdesc5 = (TextBox)FormView1.FindControl("vBomItemDscr5TextBox");
            //TextBox itemdesc6 = (TextBox)FormView1.FindControl("vBomItemDscr6TextBox");
            //TextBox itemdesc7 = (TextBox)FormView1.FindControl("vBomItemDscr7TextBox");
            //TextBox itemdesc8 = (TextBox)FormView1.FindControl("vBomItemDscr8TextBox");
            //TextBox itemdesc9 = (TextBox)FormView1.FindControl("vBomItemDscr9TextBox");
            //TextBox itemdesc10 = (TextBox)FormView1.FindControl("vBomItemDscr10TextBox");
            TextBox shrink1 = (TextBox)FormView1.FindControl("vShrink1TextBox");
            TextBox shrink2 = (TextBox)FormView1.FindControl("vShrink2TextBox");
            TextBox shrink3 = (TextBox)FormView1.FindControl("vShrink3TextBox");
            TextBox shrink4 = (TextBox)FormView1.FindControl("vShrink4TextBox");
            TextBox shrink5 = (TextBox)FormView1.FindControl("vShrink5TextBox");
            TextBox shrink6 = (TextBox)FormView1.FindControl("vShrink6TextBox");
            TextBox shrink7 = (TextBox)FormView1.FindControl("vShrink7TextBox");
            TextBox shrink8 = (TextBox)FormView1.FindControl("vShrink8TextBox");
            //TextBox sqinch1 = (TextBox)FormView1.FindControl("vSqInch1TextBox");
            //TextBox sqinch2 = (TextBox)FormView1.FindControl("vSqInch2TextBox");
            //TextBox sqinch3 = (TextBox)FormView1.FindControl("vSqInch3TextBox");
            //TextBox sqinch4 = (TextBox)FormView1.FindControl("vSqInch4TextBox");
            //TextBox sqinch5 = (TextBox)FormView1.FindControl("vSqInch5TextBox");
            //TextBox sqinch6 = (TextBox)FormView1.FindControl("vSqInch6TextBox");
            //TextBox sqinch7 = (TextBox)FormView1.FindControl("vSqInch7TextBox");
            //TextBox sqinch8 = (TextBox)FormView1.FindControl("vSqInch8TextBox");
            //TextBox sqinch9 = (TextBox)FormView1.FindControl("vSqInch9TextBox");
            //TextBox sqinch10 = (TextBox)FormView1.FindControl("vSqInch10TextBox");

            UserClass UserLogin = (UserClass)Session["User"];

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
            ////ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_corrugated_est"]);
            ////ObjectDataSource1.SelectParameters["prmForm"].DefaultValue =Convert.ToString(Session["order_corrugated_formno"]);
            ObjectDataSource1.SelectParameters["prmBomItem1"].DefaultValue = item1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBomItem2"].DefaultValue = item2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBomItem3"].DefaultValue = item3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBomItem4"].DefaultValue = item4.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBomItem5"].DefaultValue = item5.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBomItem6"].DefaultValue = item6.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBomItem7"].DefaultValue = item7.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBomItem8"].DefaultValue = item8.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLamCode"].DefaultValue = lamcode.Text.Trim();
            ObjectDataSource1.SelectParameters["prmAdhesive"].DefaultValue = adhesive.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmBomItemDscr1"].DefaultValue = itemdesc1.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmBomItemDscr2"].DefaultValue = itemdesc2.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmBomItemDscr3"].DefaultValue = itemdesc3.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmBomItemDscr4"].DefaultValue = itemdesc4.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmBomItemDscr5"].DefaultValue = itemdesc5.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmBomItemDscr6"].DefaultValue = itemdesc6.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmBomItemDscr7"].DefaultValue = itemdesc7.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmBomItemDscr8"].DefaultValue = itemdesc8.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmBomItemDscr9"].DefaultValue = itemdesc9.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmBomItemDscr10"].DefaultValue = itemdesc10.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShrink1"].DefaultValue = shrink1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShrink2"].DefaultValue = shrink2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShrink3"].DefaultValue = shrink3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShrink4"].DefaultValue = shrink4.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShrink5"].DefaultValue = shrink5.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShrink6"].DefaultValue = shrink6.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShrink7"].DefaultValue = shrink7.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShrink8"].DefaultValue = shrink8.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSqInch1"].DefaultValue = sqinch1.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSqInch2"].DefaultValue = sqinch2.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSqInch3"].DefaultValue = sqinch3.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSqInch4"].DefaultValue = sqinch4.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSqInch5"].DefaultValue = sqinch5.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSqInch6"].DefaultValue = sqinch6.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSqInch7"].DefaultValue = sqinch7.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSqInch8"].DefaultValue = sqinch8.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSqInch9"].DefaultValue = sqinch9.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSqInch10"].DefaultValue = sqinch10.Text.Trim();
            FormView1.ChangeMode(FormViewMode.Edit);
        }
        catch { }
    }
}
