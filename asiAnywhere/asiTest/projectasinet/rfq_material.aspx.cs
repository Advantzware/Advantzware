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
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class rfqmaterial : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        
        Session["my_new_rfq"] = null;
        //ImageButton rfqmaterial = (ImageButton)Master.FindControl("rfq_material");
        //rfqmaterial.ImageUrl = "~/Images/rfqmaterials1.jpg";
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Rfq Material";
        FormView1.ChangeMode(FormViewMode.ReadOnly);

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {


                string vUserId = UserLogin.UserName;
                string vPage = "rfq_material.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;


                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                //lblComp.Text = PrmComp;
                //Response.Write(vCanRun);
                if (vCanRun == true)
                {
                    //lnk_brwsorder.Visible = true;
                    //brwsorder.Visible = true;

                }
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }
            }
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "";

        }
    }
    protected void update_RfqMaterial(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "rfq_material.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            if (vCanUpdate == false)
            {
                Response.Write("<script>alert('Sorry! You do not have permission to Update Record');</script>");
                Response.Write("<script>window.location.href = 'rfq_material.aspx';</script>");


            }

        }

        //string swhere = Request.Form["Rfqradio"];
        //if (swhere != "" && swhere != null)
        //{
        //    ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "SelectRow";
        //    //ObjectDataSource1.SelectParameters["RfqRowid"].DefaultValue = swhere;
        //    Session["PrmPartNo"] = Session["PrmPartNo"];
        //}
        //else
        //{
        //    Response.Write("<script>alert('No Record to Update')</script>");
        //    Response.Write("<script>window.location.href = 'rfq_material.aspx';</script>");

        //}
    }
    protected void updateRfqMaterial(object sender, EventArgs e)
    {


        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "UpdateRfqMaterial";


        Session["rfqmatno"] = Session["rfqmatno"];



        TextBox prmBoard = (TextBox)FormView1.FindControl("vBoardTextBox");
        TextBox prmBrdDscr = (TextBox)FormView1.FindControl("vBrdDscrTextBox");
        TextBox prmCal = (TextBox)FormView1.FindControl("vCalTextBox");
        TextBox prmGshwid = (TextBox)FormView1.FindControl("vGshwidTextBox");
        TextBox prmGshlen = (TextBox)FormView1.FindControl("vGshlenTextBox");

        TextBox prmLeaf1 = (TextBox)FormView1.FindControl("vLeaf1TextBox");
        TextBox prmLeafw1 = (TextBox)FormView1.FindControl("vLeafw1TextBox");
        TextBox prmLeafl1 = (TextBox)FormView1.FindControl("vLeafl1TextBox");

        TextBox prmLeaf2 = (TextBox)FormView1.FindControl("vLeaf2TextBox");
        TextBox prmLeafw2 = (TextBox)FormView1.FindControl("vLeafw2TextBox");
        TextBox prmLeafl2 = (TextBox)FormView1.FindControl("vLeafl2TextBox");

        TextBox prmLeaf3 = (TextBox)FormView1.FindControl("vLeaf3TextBox");
        TextBox prmvLeafw3 = (TextBox)FormView1.FindControl("vLeafw3TextBox");
        TextBox prmLeafl3 = (TextBox)FormView1.FindControl("vLeafl3TextBox");

        TextBox prmLeaf4 = (TextBox)FormView1.FindControl("vLeaf4TextBox");
        TextBox prmLeafw4 = (TextBox)FormView1.FindControl("vLeafw4TextBox");
        TextBox prmLeafl4 = (TextBox)FormView1.FindControl("vLeafl4TextBox");

        TextBox prmSpecdscr1 = (TextBox)FormView1.FindControl("vSpecdscr1TextBox");

        TextBox prmSpecdscr2 = (TextBox)FormView1.FindControl("vSpecdscr2TextBox");

        TextBox prmSpecdscr3 = (TextBox)FormView1.FindControl("vSpecdscr3TextBox");

        TextBox prmSpecdscr4 = (TextBox)FormView1.FindControl("vSpecdscr4TextBox");
        TextBox prmSpecdscr5 = (TextBox)FormView1.FindControl("vSpecdscr5TextBox");
        TextBox prmSpecdscr6 = (TextBox)FormView1.FindControl("vSpecdscr6TextBox");
        TextBox prmAdder1 = (TextBox)FormView1.FindControl("vAdder1TextBox");
        TextBox prmAdder2 = (TextBox)FormView1.FindControl("vAdder2TextBox");
        TextBox prmAdder3 = (TextBox)FormView1.FindControl("vAdder3TextBox");
        TextBox prmAdder4 = (TextBox)FormView1.FindControl("vAdder4TextBox");
        TextBox prmAdder5 = (TextBox)FormView1.FindControl("vAdder5TextBox");
        TextBox prmAdder6 = (TextBox)FormView1.FindControl("vAdder6TextBox");
        TextBox prmAdder7 = (TextBox)FormView1.FindControl("vAdder7TextBox");
        TextBox prmAdder8 = (TextBox)FormView1.FindControl("vAdder8TextBox");
        TextBox prmAdder9 = (TextBox)FormView1.FindControl("vAdder9TextBox");
        TextBox prmAdder10 = (TextBox)FormView1.FindControl("vAdder10TextBox");
        TextBox prmAdder11 = (TextBox)FormView1.FindControl("vAdder11TextBox");
        TextBox prmAdder12 = (TextBox)FormView1.FindControl("vAdder12TextBox");

        TextBox prmSpecno1 = (TextBox)FormView1.FindControl("vSpecno1TextBox");
        TextBox prmSpecno2 = (TextBox)FormView1.FindControl("vSpecno2TextBox");

        TextBox prmSpecno3 = (TextBox)FormView1.FindControl("vSpecno3TextBox");
        TextBox prmSpecno4 = (TextBox)FormView1.FindControl("vSpecno4TextBox");
        TextBox prmSpecno5 = (TextBox)FormView1.FindControl("vSpecno5TextBox");
        TextBox prmSpecno6 = (TextBox)FormView1.FindControl("vSpecno6TextBox");
        TextBox prmLeafdscr = (TextBox)FormView1.FindControl("vLeafdscrTextBox");
        TextBox prmLeafdscr2 = (TextBox)FormView1.FindControl("vLeafdscr2TextBox");
        TextBox prmLeafdscr3 = (TextBox)FormView1.FindControl("vLeafdscr3TextBox");
        TextBox prmLeafdscr4 = (TextBox)FormView1.FindControl("vLeafdscr4TextBox");
        TextBox prmSpecQty = (TextBox)FormView1.FindControl("vSpecQtyTextBox");
        TextBox prmSpecQty2 = (TextBox)FormView1.FindControl("vSpecQty2TextBox");

        TextBox prmSpecQty3 = (TextBox)FormView1.FindControl("vSpecQty3TextBox");
        TextBox vSpecQty4 = (TextBox)FormView1.FindControl("vSpecQty4TextBox");
        TextBox prmSpecQty5 = (TextBox)FormView1.FindControl("vSpecQty5TextBox");
        TextBox prmSpecQty6 = (TextBox)FormView1.FindControl("vSpecQty6TextBox");
                
        //ObjectDataSource1.SelectParameters["RfqSeqNo"].DefaultValue = RefSeqNo.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBoard"].DefaultValue = prmBoard.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBrdDscr"].DefaultValue = prmBrdDscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCal"].DefaultValue = prmCal.Text.Trim();
        ObjectDataSource1.SelectParameters["prmGshwid"].DefaultValue = prmGshwid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmGshlen"].DefaultValue = prmGshlen.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeaf1"].DefaultValue = prmLeaf1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeafw1"].DefaultValue = prmLeafw1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeafl1"].DefaultValue = prmLeafl1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeaf2"].DefaultValue = prmLeaf2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeafw2"].DefaultValue = prmLeafw2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeafl2"].DefaultValue = prmLeafl2.Text.Trim();


        ObjectDataSource1.SelectParameters["prmLeaf3"].DefaultValue = prmLeaf3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvLeafw3"].DefaultValue = prmvLeafw3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeafl3"].DefaultValue = prmLeafl3.Text.Trim();

        ObjectDataSource1.SelectParameters["prmLeaf4"].DefaultValue = prmLeaf4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeafw4"].DefaultValue = prmLeafw4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeafl4"].DefaultValue = prmLeafl4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecdscr1"].DefaultValue = prmSpecdscr1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecdscr2"].DefaultValue = prmSpecdscr2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecdscr3"].DefaultValue = prmSpecdscr3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecdscr4"].DefaultValue = prmSpecdscr4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecdscr5"].DefaultValue = prmSpecdscr5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecdscr6"].DefaultValue = prmSpecdscr6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdder1"].DefaultValue = prmAdder1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdder2"].DefaultValue = prmAdder2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdder3"].DefaultValue = prmAdder3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdder4"].DefaultValue = prmAdder4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdder5"].DefaultValue = prmAdder5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdder6"].DefaultValue = prmAdder6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdder7"].DefaultValue = prmAdder7.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdder8"].DefaultValue = prmAdder8.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdder9"].DefaultValue = prmAdder9.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdder10"].DefaultValue = prmAdder10.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdder11"].DefaultValue = prmAdder11.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdder12"].DefaultValue = prmAdder12.Text.Trim();

        ObjectDataSource1.SelectParameters["prmSpecno1"].DefaultValue = prmSpecno1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecno2"].DefaultValue = prmSpecno2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecno3"].DefaultValue = prmSpecno3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecno4"].DefaultValue = prmSpecno4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecno5"].DefaultValue = prmSpecno5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecno6"].DefaultValue = prmSpecno6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeafdscr"].DefaultValue = prmLeafdscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeafdscr2"].DefaultValue = prmLeafdscr2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeafdscr3"].DefaultValue = prmLeafdscr3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLeafdscr4"].DefaultValue = prmLeafdscr4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecQty"].DefaultValue = prmSpecQty.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecQty2"].DefaultValue = prmSpecQty2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecQty3"].DefaultValue = prmSpecQty3.Text.Trim();
        ObjectDataSource1.SelectParameters["vSpecQty4"].DefaultValue = vSpecQty4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecQty5"].DefaultValue = prmSpecQty5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpecQty6"].DefaultValue = prmSpecQty6.Text.Trim();
        //ObjectDataSource1.SelectParameters["MatRowid"].DefaultValue = MatRowid.Text.Trim();

        //decimal width1 = Convert.ToDecimal(prmGshwid.Text.Trim());
        //decimal length1 = Convert.ToDecimal(prmGshlen.Text.Trim());
        //decimal width2 = Convert.ToDecimal(prmLeafw1.Text.Trim());
        //decimal length2 = Convert.ToDecimal(prmLeafl1.Text.Trim());
        //decimal width3 = Convert.ToDecimal(prmLeafw2.Text.Trim());
        //decimal length3 = Convert.ToDecimal(prmLeafl2.Text.Trim());
        //decimal width4 = Convert.ToDecimal(prmvLeafw3.Text.Trim());
        //decimal length4 = Convert.ToDecimal(prmLeafl3.Text.Trim());
        //decimal width5 = Convert.ToDecimal(prmLeafw4.Text.Trim());
        //decimal length5 = Convert.ToDecimal(prmLeafl4.Text.Trim());
        //decimal qty1 = Convert.ToDecimal(prmSpecQty.Text.Trim());
        //decimal qty2 = Convert.ToDecimal(prmSpecQty2.Text.Trim());
        //decimal qty3 = Convert.ToDecimal(prmSpecQty3.Text.Trim());
        //decimal qty4 = Convert.ToDecimal(vSpecQty4.Text.Trim());
        //decimal qty5 = Convert.ToDecimal(prmSpecQty5.Text.Trim());
        //decimal qty6 = Convert.ToDecimal(prmSpecQty6.Text.Trim());







        //if (prmBoard.Text.Trim() != "")
        //{
        //    if (width1 <= 0 || length1 <= 0)
        //    {
        //        Response.Write("<script>alert('Length and width must have some value')</script>");
        //        FormView1.ChangeMode(FormViewMode.Edit);
        //    }
        //}
        //if (prmLeaf1.Text.Trim() != "")
        //{
        //    if (width2 <= 0 || length2 <= 0)
        //    {
        //        Response.Write("<script>alert('Length and width must have some value')</script>");
        //        FormView1.ChangeMode(FormViewMode.Edit);
        //    }
        //}
        //if (prmLeaf2.Text.Trim() != "")
        //{
        //    if (width3 <= 0 || length3 <= 0)
        //    {
        //        Response.Write("<script>alert('Length and width must have some value')</script>");
        //        FormView1.ChangeMode(FormViewMode.Edit);
        //    }
        //}
        //if (prmLeaf3.Text.Trim() != "")
        //{
        //    if (width4 <= 0 || length4 <= 0)
        //    {
        //        Response.Write("<script>alert('Length and width must have some value')</script>");
        //        FormView1.ChangeMode(FormViewMode.Edit);
        //    }
        //}
        //if (prmLeaf4.Text.Trim() != "")
        //{
        //    if (width5 <= 0 || length5 <= 0)
        //    {
        //        Response.Write("<script>alert('Length and width must have some value')</script>");
        //        FormView1.ChangeMode(FormViewMode.Edit);
        //    }
        //}
        //if (prmSpecno1.Text.Trim() != "")
        //{
        //    if (qty1 <= 0)
        //    {
        //        Response.Write("<script>alert('Length and width must have some value')</script>");
        //        FormView1.ChangeMode(FormViewMode.Edit);
        //    }
        //}
        //if (prmSpecno2.Text.Trim() != "")
        //{
        //    if (qty2 <= 0)
        //    {
        //        Response.Write("<script>alert('Length and width must have some value')</script>");
        //        FormView1.ChangeMode(FormViewMode.Edit);
        //    }
        //}
        //if (prmSpecno3.Text.Trim() != "")
        //{
        //    if (qty3 <= 0)
        //    {
        //        Response.Write("<script>alert('Length and width must have some value')</script>");
        //        FormView1.ChangeMode(FormViewMode.Edit);
        //    }
        //}
        //if (prmSpecno4.Text.Trim() != "")
        //{
        //    if (qty4 <= 0)
        //    {
        //        Response.Write("<script>alert('Length and width must have some value')</script>");
        //        FormView1.ChangeMode(FormViewMode.Edit);
        //    }
        //}
        //if (prmSpecno5.Text.Trim() != "")
        //{
        //    if (qty5 <= 0)
        //    {
        //        Response.Write("<script>alert('Length and width must have some value')</script>");
        //        FormView1.ChangeMode(FormViewMode.Edit);
        //    }
        //}
        //if (prmSpecno6.Text.Trim() != "")
        //{
        //    if (qty6 <= 0)
        //    {
        //        Response.Write("<script>alert('Length and width must have some value')</script>");
        //        FormView1.ChangeMode(FormViewMode.Edit);
        //    }
        //}

    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox board = (TextBox)FormView1.FindControl("vBoardTextBox");
            board.Focus();
                
        }
    }

}