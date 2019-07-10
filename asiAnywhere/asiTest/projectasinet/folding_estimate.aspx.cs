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

public partial class folding_estimate : System.Web.UI.Page
{
    
    string cheksave = null;
    protected void Page_PreRender(object sender, EventArgs e)
    {     
       
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        HiddenField4.Value = "0";
        //FormView_CorrugatedEstimate.ChangeMode(FormViewMode.ReadOnly);
        CorrugatedEstimateDataSource.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource_list.SelectParameters["prmAction"].DefaultValue = "ListSelect";
        GridView1.DataBind();
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            Label vartype = (Label)FormView_CorrugatedEstimate.FindControl("vEstTypeLabel");
            Session["order_est_foldtype"] = vartype.Text;
                        
            Image img_mov_col = (Image)Master.FindControl("Image5");
            img_mov_col.Visible = false;

            if (Session["fold_grid_index"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["fold_grid_index"]);
            }
        }
        catch { }        

        if (!Page.IsPostBack)
        {            
            if (Convert.ToString(Session["order_folding_est"]) == null)
            {
                newaddButton.Visible = true;
            }
            else
            {
                newaddButton.Visible = false;
            }
            if (Convert.ToString(Session["add_est_from_buton_fold"]) == "add")
            {
                FormView_CorrugatedEstimate.ChangeMode(FormViewMode.Insert);
            }
        }
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "corrugated_estimate.aspx";
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
            labelname.Text = "Foldings";
            if (labelname.Text == "Foldings")
            {
                Session["CorrugatedCartoon"] = null;
                Session["FoldingCartoon"] = 1;
                
            }
            if (aUsers == "external")
            {
                
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        
    }
    protected void save_corr_Click(object sender, EventArgs e)
    {
        if (cheksave != "Yes")            
        {
            Label newest = (Label)FormView_CorrugatedEstimate.FindControl("EatimateLabel");
            RadioButtonList type = (RadioButtonList)FormView_CorrugatedEstimate.FindControl("RadioButtonList1");
            TextBox cust = (TextBox)FormView_CorrugatedEstimate.FindControl("vCustTextBox");
            TextBox fgitem = (TextBox)FormView_CorrugatedEstimate.FindControl("vFgItemTextBox");
            TextBox custpart = (TextBox)FormView_CorrugatedEstimate.FindControl("vCustPartTextBox");
            TextBox shipto = (TextBox)FormView_CorrugatedEstimate.FindControl("vShipToTextBox");
            TextBox itemname = (TextBox)FormView_CorrugatedEstimate.FindControl("vItemNameTextBox");
            TextBox estqty = (TextBox)FormView_CorrugatedEstimate.FindControl("vEstQtyTextBox");
            TextBox style = (TextBox)FormView_CorrugatedEstimate.FindControl("vStyleTextBox");
            TextBox flute = (TextBox)FormView_CorrugatedEstimate.FindControl("vFluteTextBox");
            TextBox test = (TextBox)FormView_CorrugatedEstimate.FindControl("vTestTextBox");
            TextBox board = (TextBox)FormView_CorrugatedEstimate.FindControl("vBoardTextBox");

            TextBox caliper = (TextBox)FormView_CorrugatedEstimate.FindControl("vCaliperTextBox");
            TextBox categ = (TextBox)FormView_CorrugatedEstimate.FindControl("vCategoryTextBox");
            TextBox length = (TextBox)FormView_CorrugatedEstimate.FindControl("vLenghtTextBox");
            TextBox width = (TextBox)FormView_CorrugatedEstimate.FindControl("vWidthTextBox");
            TextBox depth = (TextBox)FormView_CorrugatedEstimate.FindControl("vDepthTextBox");
            //TextBox form = (TextBox)FormView_CorrugatedEstimate.FindControl("vFormTextBox");
            //TextBox blank = (TextBox)FormView_CorrugatedEstimate.FindControl("vBlankTextBox");        
            TextBox color = (TextBox)FormView_CorrugatedEstimate.FindControl("vColorTextBox");
            //TextBox pass = (TextBox)FormView_CorrugatedEstimate.FindControl("vPassesTextBox");

            TextBox coating = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatingTextBox");
            //TextBox coatpass = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatPassesTextBox");
            TextBox qtyset = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtySetTextBox");
            //TextBox inkform = (TextBox)FormView_CorrugatedEstimate.FindControl("vInkFromTextBox");
            //TextBox passform = (TextBox)FormView_CorrugatedEstimate.FindControl("vPassesFromTextBox");
            //TextBox coatingform = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatingFromTextBox");
            //TextBox coatpassform = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatPassesFromTextBox");
            TextBox diein = (TextBox)FormView_CorrugatedEstimate.FindControl("vDieInTextBox");
            DropDownList purch = (DropDownList)FormView_CorrugatedEstimate.FindControl("DropDownList2");
            TextBox estdate = (TextBox)FormView_CorrugatedEstimate.FindControl("vEstDateTextBox");
            TextBox qty1 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent1TextBox");
            TextBox qty2 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent2TextBox");
            TextBox qty3 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent3TextBox");
            TextBox qty4 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent4TextBox");
            TextBox qty5 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent5TextBox");
            TextBox qty6 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent6TextBox");
            TextBox qty7 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent7TextBox");
            TextBox qty8 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent8TextBox");
            TextBox qty9 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent9TextBox");
            TextBox qty10 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent10TextBox");
            TextBox qty11 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent11TextBox");
            TextBox qty12 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent12TextBox");
            TextBox qty13 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent13TextBox");
            TextBox qty14 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent14TextBox");
            TextBox qty15 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent15TextBox");


            TextBox relqty1 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent1TextBox");
            TextBox relqty2 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent2TextBox");
            TextBox relqty3 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent3TextBox");
            TextBox relqty4 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent4TextBox");
            TextBox relqty5 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent5TextBox");
            TextBox relqty6 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent6TextBox");
            TextBox relqty7 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent7TextBox");
            TextBox relqty8 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent8TextBox");
            TextBox relqty9 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent9TextBox");
            TextBox relqty10 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent10TextBox");
            TextBox relqty11 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent11TextBox");
            TextBox relqty12 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent12TextBox");
            TextBox relqty13 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent13TextBox");
            TextBox relqty14 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent14TextBox");
            TextBox relqty15 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent15TextBox");

            if (type.SelectedIndex == 0)
            {
                HiddenField1.Value = "Single";
            }
            if (type.SelectedIndex == 1)
            {
                HiddenField1.Value = "Set";
            }
            if (type.SelectedIndex == 2)
            {
                HiddenField1.Value = "Tandem";
            }

            if (purch.SelectedIndex == 0)
            {
                HiddenField3.Value = "M";
            }
            if (purch.SelectedIndex == 1)
            {
                HiddenField3.Value = "P";
            }


            if (length.Text == "")
                length.Text = "0";
            if (width.Text == "")
                width.Text = "0";
            if (depth.Text == "")
                depth.Text = "0";
            if (color.Text == "")
                color.Text = "0";
            if (coating.Text == "")
                coating.Text = "0";
            if (diein.Text == "")
                diein.Text = "0";
            if (qtyset.Text == "")
                qtyset.Text = "0";
            if (HiddenFieldCaliper.Value == "" || HiddenFieldCaliper.Value == null)
                HiddenFieldCaliper.Value = "0";

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated corr = new Corrugated();

            bool check = corr.ValidateFodingEst("Add", UserLogin.UserName, Convert.ToString(Session["order_folding_est"]), cust.Text.Trim(), custpart.Text.Trim(), shipto.Text.Trim(), itemname.Text.Trim(), fgitem.Text.Trim(), Convert.ToDecimal(estqty.Text.Trim()), style.Text.Trim(), flute.Text.Trim(), test.Text.Trim(), board.Text.Trim(), 0, categ.Text.Trim(), Convert.ToDecimal(length.Text.Trim()), Convert.ToDecimal(width.Text.Trim()), Convert.ToDecimal(depth.Text.Trim()), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(Session["order_folding_blankno"]), type.Text.Trim(), Convert.ToInt32(color.Text.Trim()), 0, Convert.ToInt32(coating.Text.Trim()), 0, Convert.ToDecimal(qtyset.Text.Trim()), 0, 0, 0, 0, purch.Text.Trim(), Convert.ToDateTime("11/11/2009"), HiddenField1.Value, 0);

            string value = Convert.ToString(check);
            if (value == "True")
            {
                Session["order_folding_est"] = newest.Text.Trim();

                CorrugatedEstimateDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                CorrugatedEstimateDataSource.SelectParameters["prmAction"].DefaultValue = "Add";
                CorrugatedEstimateDataSource.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_folding_est"]);
                CorrugatedEstimateDataSource.SelectParameters["prmCust"].DefaultValue = cust.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmCustPart"].DefaultValue = custpart.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmShipTo"].DefaultValue = shipto.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmItemName"].DefaultValue = itemname.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmFgItem"].DefaultValue = fgitem.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty"].DefaultValue = estqty.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmStyle"].DefaultValue = style.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmPaper1"].DefaultValue = flute.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmPaper2"].DefaultValue = test.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmBoard"].DefaultValue = board.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmCalliper"].DefaultValue = HiddenFieldCaliper.Value;
                CorrugatedEstimateDataSource.SelectParameters["prmCategory"].DefaultValue = categ.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmLength"].DefaultValue = length.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmWidth"].DefaultValue = width.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmDepth"].DefaultValue = depth.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmFrom"].DefaultValue = form.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmBlank"].DefaultValue = blank.Text.Trim();

                CorrugatedEstimateDataSource.SelectParameters["prmColor"].DefaultValue = color.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmPasses"].DefaultValue = pass.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmCoating"].DefaultValue = coating.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmCoatPasses"].DefaultValue = coatpass.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmQtySet"].DefaultValue = qtyset.Text.Trim();

                //CorrugatedEstimateDataSource.SelectParameters["prmInkFrom"].DefaultValue = inkform.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmPassesFrom"].DefaultValue = passform.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmCoatingFrom"].DefaultValue = coatingform.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmCoatPassesFrom"].DefaultValue = coatpassform.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmPurchManuf"].DefaultValue = HiddenField3.Value;
                CorrugatedEstimateDataSource.SelectParameters["prmEstDate"].DefaultValue = estdate.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmType"].DefaultValue = HiddenField1.Value;
                CorrugatedEstimateDataSource.SelectParameters["prmDiein"].DefaultValue = diein.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty2"].DefaultValue = qty2.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty3"].DefaultValue = qty3.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty4"].DefaultValue = qty4.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty5"].DefaultValue = qty5.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty6"].DefaultValue = qty6.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty7"].DefaultValue = qty7.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty8"].DefaultValue = qty8.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty9"].DefaultValue = qty9.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty10"].DefaultValue = qty10.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty11"].DefaultValue = qty11.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty12"].DefaultValue = qty12.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty13"].DefaultValue = qty13.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty14"].DefaultValue = qty14.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty15"].DefaultValue = qty15.Text.Trim();

                CorrugatedEstimateDataSource.SelectParameters["prmRelQty1"].DefaultValue = relqty1.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty2"].DefaultValue = relqty2.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty3"].DefaultValue = relqty3.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty4"].DefaultValue = relqty4.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty5"].DefaultValue = relqty5.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty6"].DefaultValue = relqty6.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty7"].DefaultValue = relqty7.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty8"].DefaultValue = relqty8.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty9"].DefaultValue = relqty9.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty10"].DefaultValue = relqty10.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty11"].DefaultValue = relqty11.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty12"].DefaultValue = relqty12.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty13"].DefaultValue = relqty13.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty14"].DefaultValue = relqty14.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty15"].DefaultValue = relqty15.Text.Trim();
                GridView1.DataBind();
                cheksave = "";
                FormView_CorrugatedEstimate.ChangeMode(FormViewMode.ReadOnly);
                Session["add_est_from_buton_fold"] = null;
                Response.Write("<script>window.location.href='folding_estimate.aspx'</script>");
            }
        }
    }
    protected void btn_insert_cancel_click(object sender, EventArgs e)
    {
        Session["add_est_from_buton_fold"] = null;
        MaintainScrollPositionOnPostBack = false;
    }

    protected void update_corr_Click(object sender, EventArgs e)
    {
        
        if (cheksave != "Yes")
        {
            Label est = (Label)FormView_CorrugatedEstimate.FindControl("vEstLabel");
            TextBox cust = (TextBox)FormView_CorrugatedEstimate.FindControl("vCustTextBox");
            TextBox fgitem = (TextBox)FormView_CorrugatedEstimate.FindControl("vFgItemTextBox");
            TextBox custpart = (TextBox)FormView_CorrugatedEstimate.FindControl("vCustPartTextBox");
            TextBox shipto = (TextBox)FormView_CorrugatedEstimate.FindControl("vShipToTextBox");
            TextBox itemname = (TextBox)FormView_CorrugatedEstimate.FindControl("vItemNameTextBox");
            TextBox estqty = (TextBox)FormView_CorrugatedEstimate.FindControl("vEstQtyTextBox");
            TextBox style = (TextBox)FormView_CorrugatedEstimate.FindControl("vStyleTextBox");
            TextBox flute = (TextBox)FormView_CorrugatedEstimate.FindControl("vFluteTextBox");
            TextBox test = (TextBox)FormView_CorrugatedEstimate.FindControl("vTestTextBox");
            TextBox board = (TextBox)FormView_CorrugatedEstimate.FindControl("vBoardTextBox");

            TextBox caliper = (TextBox)FormView_CorrugatedEstimate.FindControl("vCaliperTextBox");
            TextBox categ = (TextBox)FormView_CorrugatedEstimate.FindControl("vCategoryTextBox");
            TextBox length = (TextBox)FormView_CorrugatedEstimate.FindControl("vLenghtTextBox");
            TextBox width = (TextBox)FormView_CorrugatedEstimate.FindControl("vWidthTextBox");
            TextBox depth = (TextBox)FormView_CorrugatedEstimate.FindControl("vDepthTextBox");
            TextBox color = (TextBox)FormView_CorrugatedEstimate.FindControl("vColorTextBox");
            //TextBox pass = (TextBox)FormView_CorrugatedEstimate.FindControl("vPassesTextBox");

            TextBox coating = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatingTextBox");
            //TextBox coatpass = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatPassesTextBox");
            TextBox qtyset = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtySetTextBox");
            //TextBox inkform = (TextBox)FormView_CorrugatedEstimate.FindControl("vInkFromTextBox");
            //TextBox passform = (TextBox)FormView_CorrugatedEstimate.FindControl("vPassesFromTextBox");
            //TextBox coatingform = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatingFromTextBox");
            TextBox diein = (TextBox)FormView_CorrugatedEstimate.FindControl("vDieInTextBox");
            DropDownList purch = (DropDownList)FormView_CorrugatedEstimate.FindControl("DropDownList2");
            TextBox estdate = (TextBox)FormView_CorrugatedEstimate.FindControl("vEstDateTextBox");

            TextBox qty1 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent1TextBox");
            TextBox qty2 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent2TextBox");
            TextBox qty3 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent3TextBox");
            TextBox qty4 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent4TextBox");
            TextBox qty5 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent5TextBox");
            TextBox qty6 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent6TextBox");
            TextBox qty7 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent7TextBox");
            TextBox qty8 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent8TextBox");
            TextBox qty9 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent9TextBox");
            TextBox qty10 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent10TextBox");
            TextBox qty11 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent11TextBox");
            TextBox qty12 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent12TextBox");
            TextBox qty13 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent13TextBox");
            TextBox qty14 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent14TextBox");
            TextBox qty15 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent15TextBox");


            TextBox relqty1 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent1TextBox");
            TextBox relqty2 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent2TextBox");
            TextBox relqty3 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent3TextBox");
            TextBox relqty4 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent4TextBox");
            TextBox relqty5 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent5TextBox");
            TextBox relqty6 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent6TextBox");
            TextBox relqty7 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent7TextBox");
            TextBox relqty8 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent8TextBox");
            TextBox relqty9 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent9TextBox");
            TextBox relqty10 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent10TextBox");
            TextBox relqty11 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent11TextBox");
            TextBox relqty12 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent12TextBox");
            TextBox relqty13 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent13TextBox");
            TextBox relqty14 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent14TextBox");
            TextBox relqty15 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent15TextBox");

            if (purch.SelectedIndex == 0)
            {
                HiddenField3.Value = "M";
            }
            if (purch.SelectedIndex == 1)
            {
                HiddenField3.Value = "P";
            }
            if (length.Text == "")
                length.Text = "0";
            if (width.Text == "")
                width.Text = "0";
            if (depth.Text == "")
                depth.Text = "0";
            if (color.Text == "")
                color.Text = "0";
            if (coating.Text == "")
                coating.Text = "0";
            if (diein.Text == "")
                diein.Text = "0";
            if (qtyset.Text == "")
                qtyset.Text = "0";

            if (HiddenFieldCaliper.Value == "" || HiddenFieldCaliper.Value == null)
                HiddenFieldCaliper.Value = "0";

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated corr = new Corrugated();

            bool check = corr.ValidateFodingEst("Update", UserLogin.UserName, Convert.ToString(Session["order_folding_est"]), cust.Text.Trim(), custpart.Text.Trim(), shipto.Text.Trim(), itemname.Text.Trim(), fgitem.Text.Trim(), Convert.ToDecimal(estqty.Text.Trim()), style.Text.Trim(), flute.Text.Trim(), test.Text.Trim(), board.Text.Trim(), 0, categ.Text.Trim(), Convert.ToDecimal(length.Text.Trim()), Convert.ToDecimal(width.Text.Trim()), Convert.ToDecimal(depth.Text.Trim()), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(Session["order_folding_blankno"]), "", Convert.ToInt32(color.Text.Trim()), 0, Convert.ToInt32(coating.Text.Trim()), 0, Convert.ToDecimal(qtyset.Text.Trim()), 0, 0, 0, 0, purch.Text.Trim(), Convert.ToDateTime("11/11/2009"), HiddenField1.Value, 0);

            string value = Convert.ToString(check);
            if (value == "True")
            {
                CorrugatedEstimateDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                CorrugatedEstimateDataSource.SelectParameters["prmAction"].DefaultValue = "Update";
                CorrugatedEstimateDataSource.SelectParameters["prmCust"].DefaultValue = cust.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmCustPart"].DefaultValue = custpart.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmShipTo"].DefaultValue = shipto.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmItemName"].DefaultValue = itemname.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmFgItem"].DefaultValue = fgitem.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty"].DefaultValue = estqty.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmStyle"].DefaultValue = style.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmPaper1"].DefaultValue = flute.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmPaper2"].DefaultValue = test.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmBoard"].DefaultValue = board.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmCalliper"].DefaultValue = HiddenFieldCaliper.Value;
                CorrugatedEstimateDataSource.SelectParameters["prmCategory"].DefaultValue = categ.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmLength"].DefaultValue = length.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmWidth"].DefaultValue = width.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmDepth"].DefaultValue = depth.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmTab"].DefaultValue = HiddenField2.Value;
                CorrugatedEstimateDataSource.SelectParameters["prmColor"].DefaultValue = color.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmPasses"].DefaultValue = pass.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmCoating"].DefaultValue = coating.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmCoatPasses"].DefaultValue = coatpass.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmQtySet"].DefaultValue = qtyset.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmPurchManuf"].DefaultValue = HiddenField3.Value;
                CorrugatedEstimateDataSource.SelectParameters["prmEstDate"].DefaultValue = estdate.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmDiein"].DefaultValue = diein.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty2"].DefaultValue = qty2.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty3"].DefaultValue = qty3.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty4"].DefaultValue = qty4.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty5"].DefaultValue = qty5.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty6"].DefaultValue = qty6.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty7"].DefaultValue = qty7.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty8"].DefaultValue = qty8.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty9"].DefaultValue = qty9.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty10"].DefaultValue = qty10.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty11"].DefaultValue = qty11.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty12"].DefaultValue = qty12.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty13"].DefaultValue = qty13.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty14"].DefaultValue = qty14.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty15"].DefaultValue = qty15.Text.Trim();

                CorrugatedEstimateDataSource.SelectParameters["prmRelQty1"].DefaultValue = relqty1.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty2"].DefaultValue = relqty2.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty3"].DefaultValue = relqty3.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty4"].DefaultValue = relqty4.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty5"].DefaultValue = relqty5.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty6"].DefaultValue = relqty6.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty7"].DefaultValue = relqty7.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty8"].DefaultValue = relqty8.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty9"].DefaultValue = relqty9.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty10"].DefaultValue = relqty10.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty11"].DefaultValue = relqty11.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty12"].DefaultValue = relqty12.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty13"].DefaultValue = relqty13.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty14"].DefaultValue = relqty14.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty15"].DefaultValue = relqty15.Text.Trim();
                GridView1.DataBind();
                cheksave = "";
                FormView_CorrugatedEstimate.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='folding_estimate.aspx'</script>");
            }
        }
    }
    protected void delete_Click(object sender, EventArgs e)
    {
        Label est = (Label)FormView_CorrugatedEstimate.FindControl("vEstLabel");
        Label form = (Label)FormView_CorrugatedEstimate.FindControl("vFormLabel");
        Label blank = (Label)FormView_CorrugatedEstimate.FindControl("vBlankLabel");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Corrugated corr = new Corrugated();
        
        bool check =  corr.ValidateCorrEst("Delete",UserLogin.UserName, est.Text.Trim(), "","", "", "", "", 0, "", "", "", "", 0, "", 0, 0,0, Convert.ToInt32(form.Text.Trim()),Convert.ToInt32(blank.Text.Trim()),"", 0, 0, 0, 0, 0, 0,0,0,0, "", Convert.ToDateTime("01/13/001"), "", "");

        string value = Convert.ToString(check);
        if (value == "True")
        {

            CorrugatedEstimateDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            CorrugatedEstimateDataSource.SelectParameters["prmAction"].DefaultValue = "Delete";
            CorrugatedEstimateDataSource.SelectParameters["prmEstimate"].DefaultValue = est.Text.Trim();
            CorrugatedEstimateDataSource.SelectParameters["prmFrom"].DefaultValue = form.Text.Trim();
            CorrugatedEstimateDataSource.SelectParameters["prmBlank"].DefaultValue = blank.Text.Trim();
            GridView1.DataBind();
            FormView_CorrugatedEstimate.ChangeMode(FormViewMode.ReadOnly);
            Response.Write("<script>window.location.href='folding_estimate.aspx'</script>");
        }
    }

    protected void FormView_CorrugatedEstimate_PreRender(object sender, EventArgs e)
    {        
        try
        {
            Label est = (Label)FormView_CorrugatedEstimate.FindControl("vEstLabel");
            Label form = (Label)FormView_CorrugatedEstimate.FindControl("vFormLabel");
            Label blank = (Label)FormView_CorrugatedEstimate.FindControl("vBlankLabel");
            Label cal = (Label)FormView_CorrugatedEstimate.FindControl("vCaliperLabel");
            Session["order_folding_formno"] = form.Text;
            Session["order_folding_est"] = est.Text;
            Session["order_folding_blankno"] = blank.Text;
            HiddenFieldCaliper.Value = cal.Text.Trim();
        }
        catch { }
        try
        {
            Label type = (Label)FormView_CorrugatedEstimate.FindControl("vEstTypeLabel");
            Button set = (Button)FormView_CorrugatedEstimate.FindControl("set_button");
            Button tandem = (Button)FormView_CorrugatedEstimate.FindControl("combo_button");
            HtmlInputButton set_button = (HtmlInputButton)FormView_CorrugatedEstimate.FindControl("btn_set_update");

            Session["corr_est_type_eb"] = type.Text.Trim();

            if (type.Text == "1")
            {
                set.Visible = false;
                tandem.Visible = false;
                set_button.Visible = false;
            }
            if (type.Text == "2")
            {
                set.Visible = false;
                tandem.Visible = true;
            }
            if (type.Text == "4")
            {
                tandem.Visible = false;
                set.Visible = true;
                set_button.Visible = false;
            }
        }
        catch { }

    }
    protected void new_button_Click(object sender, EventArgs e)
    {
        FormView_CorrugatedEstimate.ChangeMode(FormViewMode.Insert);
        newaddButton.Visible = false;
    }
    protected void form_est_Click(object sender, EventArgs e)
    {
        Session["visible2"] = 2;
    }

    protected void btn_update_click(object sender, EventArgs e)
    {
        Session["visible2"] = 1;
    }

    protected void Gridviewdatabound(object sender, EventArgs e)
    {       
        if (Convert.ToString(Session["order_efbrows"]) == "")
        {
            Response.Redirect("folding_estimate.aspx");
        }

        if (Convert.ToString(Session["order_efbrows"]) != "Yes")
        {
            GridView1.Columns[9].Visible = false;
            GridView1.Columns[10].Visible = false;
        }       
    }

    protected void formview_databound(object sender, EventArgs e)
    {        
        if (FormView_CorrugatedEstimate.CurrentMode == FormViewMode.ReadOnly)
        {               
            GridView1.Visible = true;
            try
            {
                Label efbrws = (Label)FormView_CorrugatedEstimate.FindControl("Label_efbrws");
                Session["order_efbrows"] = efbrws.Text.Trim();                

                Label flute = (Label)FormView_CorrugatedEstimate.FindControl("vFluteLabel");
                Label test = (Label)FormView_CorrugatedEstimate.FindControl("vTestLabel");
                Label paper1 = (Label)FormView_CorrugatedEstimate.FindControl("paper1id");
                Label paper2 = (Label)FormView_CorrugatedEstimate.FindControl("paper2id");



                if (Convert.ToString(Session["order_efbrows"]) != "Yes")
                {
                    flute.Visible = false;
                    test.Visible = false;
                    paper1.Visible = false;
                    paper2.Visible = false;
                }
            }
            catch { }
                
        }
        if (FormView_CorrugatedEstimate.CurrentMode == FormViewMode.Insert)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            TextBox cust = (TextBox)FormView_CorrugatedEstimate.FindControl("vCustTextBox");
            TextBox shipto = (TextBox)FormView_CorrugatedEstimate.FindControl("vShipToTextBox");
            TextBox EstDate = (TextBox)FormView_CorrugatedEstimate.FindControl("vEstDateTextBox");
            TextBox qtyset = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtySetTextBox");

            TextBox form = (TextBox)FormView_CorrugatedEstimate.FindControl("vFormTextBox");
            TextBox blank = (TextBox)FormView_CorrugatedEstimate.FindControl("vBlankTextBox");

            TextBox flute = (TextBox)FormView_CorrugatedEstimate.FindControl("vFluteTextBox");
            TextBox test = (TextBox)FormView_CorrugatedEstimate.FindControl("vTestTextBox");
            Label paper1 = (Label)FormView_CorrugatedEstimate.FindControl("paper1id");
            Label paper2 = (Label)FormView_CorrugatedEstimate.FindControl("paper2id");
            Image img1 = (Image)FormView_CorrugatedEstimate.FindControl("FluteLook");
            Image img2 = (Image)FormView_CorrugatedEstimate.FindControl("Image9");
            
            qtyset.Text = "1";
            form.Text = "1";
            blank.Text = "1";
            EstDate.Text = System.DateTime.Now.Date.ToShortDateString();
            try
            {
                Label est = (Label)FormView_CorrugatedEstimate.FindControl("EatimateLabel");
                Corrugated corr = new Corrugated();
                DataSet ds = new DataSet();
                ds = corr.GetNewEstimate("Est", UserLogin.UserName, "");
                est.Text = ds.Tables[0].Rows[0][0].ToString();
            }
            catch { }

            if (Convert.ToString(Session["order_efbrows"]) != "Yes")
            {
                flute.Visible = false;
                test.Visible = false;
                paper1.Visible = false;
                paper2.Visible = false;
                img1.Visible = false;
                img2.Visible = false;
            }
            

            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "folding_estimate.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();

                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                if (aUsers == "external")
                {
                    try
                    {
                        string UserId = UserLogin.UserName;
                        string aDefaultCust = null;
                        string aComp = null;

                        func1 user = new func1();
                        user.CheckUserCustomer(aComp, UserId, ref aDefaultCust);                        
                        cust.Text = aDefaultCust;
                        shipto.Text = aDefaultCust;
                    }
                    catch { }
                }
            }

            EstDate.Focus();
            GridView1.Visible = false;
        }
        if (FormView_CorrugatedEstimate.CurrentMode == FormViewMode.Edit)
        {
            TextBox cust = (TextBox)FormView_CorrugatedEstimate.FindControl("vCustTextBox");
            TextBox EstDate = (TextBox)FormView_CorrugatedEstimate.FindControl("vEstDateTextBox");

            TextBox flute = (TextBox)FormView_CorrugatedEstimate.FindControl("vFluteTextBox");
            TextBox test = (TextBox)FormView_CorrugatedEstimate.FindControl("vTestTextBox");
            Label paper1 = (Label)FormView_CorrugatedEstimate.FindControl("paper1id");
            Label paper2 = (Label)FormView_CorrugatedEstimate.FindControl("paper2id");
            Image img1 = (Image)FormView_CorrugatedEstimate.FindControl("FluteLook");
            Image img2 = (Image)FormView_CorrugatedEstimate.FindControl("Image9");
            Label type = (Label)FormView_CorrugatedEstimate.FindControl("vEstTypeLabel");
            TextBox qtyset = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtySetTextBox");
                        
            if (Convert.ToString(Session["order_efbrows"]) != "Yes")
            {
                flute.Visible = false;
                test.Visible = false;
                paper1.Visible = false;
                paper2.Visible = false;
                img1.Visible = false;
                img2.Visible = false;
            }

            EstDate.Focus();            

            GridView1.Visible = false;
            Button btnupdate = (Button)FormView_CorrugatedEstimate.FindControl("UpdateButton");
            Button btnest = (Button)FormView_CorrugatedEstimate.FindControl("btn_Form_estimate");
            Button btnblank = (Button)FormView_CorrugatedEstimate.FindControl("BlankButton");
            TextBox eqty = (TextBox)FormView_CorrugatedEstimate.FindControl("vEstQtyTextBox");
            try
            {
                if (Convert.ToInt32(Session["visible2"]) == 1)
                {                   
                    btnblank.Visible = false;
                    btnest.Visible = false;
                }
                if (Convert.ToInt32(Session["visible2"]) == 2)
                {
                    btnupdate.Visible = false;
                    btnblank.Visible = false;
                }
                if (Convert.ToInt32(Session["visible2"]) == 3)
                {
                    btnest.Visible = false;
                    btnupdate.Visible = false;
                }
            }
            catch { }

            if (Convert.ToInt32(Session["order_est_foldtype"]) == 2)
            {
                if (Convert.ToString(Session["order_folding_formno"]) == "1" && Convert.ToString(Session["order_folding_blankno"]) == "1")
                {
                    //eqty.Enabled = true;
                }
                else
                    eqty.Enabled = false;
            }

        }
    }

    protected void Form_estimate_Click(object sender, EventArgs e)
    {
        if (cheksave != "Yes")
        {
            Label est = (Label)FormView_CorrugatedEstimate.FindControl("vEstLabel");
            TextBox cust = (TextBox)FormView_CorrugatedEstimate.FindControl("vCustTextBox");
            TextBox fgitem = (TextBox)FormView_CorrugatedEstimate.FindControl("vFgItemTextBox");
            TextBox custpart = (TextBox)FormView_CorrugatedEstimate.FindControl("vCustPartTextBox");
            TextBox shipto = (TextBox)FormView_CorrugatedEstimate.FindControl("vShipToTextBox");
            TextBox itemname = (TextBox)FormView_CorrugatedEstimate.FindControl("vItemNameTextBox");
            TextBox estqty = (TextBox)FormView_CorrugatedEstimate.FindControl("vEstQtyTextBox");
            TextBox style = (TextBox)FormView_CorrugatedEstimate.FindControl("vStyleTextBox");
            TextBox flute = (TextBox)FormView_CorrugatedEstimate.FindControl("vFluteTextBox");
            TextBox test = (TextBox)FormView_CorrugatedEstimate.FindControl("vTestTextBox");
            TextBox board = (TextBox)FormView_CorrugatedEstimate.FindControl("vBoardTextBox");
            TextBox caliper = (TextBox)FormView_CorrugatedEstimate.FindControl("vCaliperTextBox");
            TextBox categ = (TextBox)FormView_CorrugatedEstimate.FindControl("vCategoryTextBox");
            TextBox length = (TextBox)FormView_CorrugatedEstimate.FindControl("vLenghtTextBox");
            TextBox width = (TextBox)FormView_CorrugatedEstimate.FindControl("vWidthTextBox");
            TextBox depth = (TextBox)FormView_CorrugatedEstimate.FindControl("vDepthTextBox");
            TextBox color = (TextBox)FormView_CorrugatedEstimate.FindControl("vColorTextBox");
            //TextBox pass = (TextBox)FormView_CorrugatedEstimate.FindControl("vPassesTextBox");

            TextBox coating = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatingTextBox");
            //TextBox coatpass = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatPassesTextBox");
            TextBox qtyset = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtySetTextBox");
            //TextBox inkform = (TextBox)FormView_CorrugatedEstimate.FindControl("vInkFromTextBox");
            //TextBox passform = (TextBox)FormView_CorrugatedEstimate.FindControl("vPassesFromTextBox");
            //TextBox coatingform = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatingFromTextBox");
            TextBox diein = (TextBox)FormView_CorrugatedEstimate.FindControl("vDieInTextBox");
            DropDownList purch = (DropDownList)FormView_CorrugatedEstimate.FindControl("DropDownList2");
            TextBox estdate = (TextBox)FormView_CorrugatedEstimate.FindControl("vEstDateTextBox");

            TextBox qty1 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent1TextBox");
            TextBox qty2 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent2TextBox");
            TextBox qty3 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent3TextBox");
            TextBox qty4 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent4TextBox");
            TextBox qty5 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent5TextBox");
            TextBox qty6 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent6TextBox");
            TextBox qty7 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent7TextBox");
            TextBox qty8 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent8TextBox");
            TextBox qty9 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent9TextBox");
            TextBox qty10 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent10TextBox");
            TextBox qty11 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent11TextBox");
            TextBox qty12 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent12TextBox");
            TextBox qty13 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent13TextBox");
            TextBox qty14 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent14TextBox");
            TextBox qty15 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent15TextBox");


            TextBox relqty1 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent1TextBox");
            TextBox relqty2 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent2TextBox");
            TextBox relqty3 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent3TextBox");
            TextBox relqty4 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent4TextBox");
            TextBox relqty5 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent5TextBox");
            TextBox relqty6 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent6TextBox");
            TextBox relqty7 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent7TextBox");
            TextBox relqty8 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent8TextBox");
            TextBox relqty9 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent9TextBox");
            TextBox relqty10 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent10TextBox");
            TextBox relqty11 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent11TextBox");
            TextBox relqty12 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent12TextBox");
            TextBox relqty13 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent13TextBox");
            TextBox relqty14 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent14TextBox");
            TextBox relqty15 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent15TextBox");

            if (purch.SelectedIndex == 0)
            {
                HiddenField3.Value = "M";
            }
            if (purch.SelectedIndex == 1)
            {
                HiddenField3.Value = "P";
            }
            if (length.Text == "")
                length.Text = "0";
            if (width.Text == "")
                width.Text = "0";
            if (depth.Text == "")
                depth.Text = "0";
            if (color.Text == "")
                color.Text = "0";
            if (coating.Text == "")
                coating.Text = "0";
            if (diein.Text == "")
                diein.Text = "0";
            if (qtyset.Text == "")
                qtyset.Text = "0";

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated corr = new Corrugated();

            bool check = corr.ValidateFodingEst("FormEstimate", UserLogin.UserName, Convert.ToString(Session["order_folding_est"]), cust.Text.Trim(), custpart.Text.Trim(), shipto.Text.Trim(), itemname.Text.Trim(), fgitem.Text.Trim(), Convert.ToDecimal(estqty.Text.Trim()), style.Text.Trim(), flute.Text.Trim(), test.Text.Trim(), board.Text.Trim(), 0, categ.Text.Trim(), Convert.ToDecimal(length.Text.Trim()), Convert.ToDecimal(width.Text.Trim()), Convert.ToDecimal(depth.Text.Trim()), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(Session["order_folding_blankno"]), "", Convert.ToInt32(color.Text.Trim()), 0, Convert.ToInt32(coating.Text.Trim()), 0, Convert.ToDecimal(qtyset.Text.Trim()), 0, 0, 0, 0, purch.Text.Trim(), Convert.ToDateTime(estdate.Text.Trim()), HiddenField1.Value, 0);

            string value = Convert.ToString(check);
            if (value == "True")
            {

                CorrugatedEstimateDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                CorrugatedEstimateDataSource.SelectParameters["prmAction"].DefaultValue = "FormEstimate";
                CorrugatedEstimateDataSource.SelectParameters["prmCust"].DefaultValue = cust.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmCustPart"].DefaultValue = custpart.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmShipTo"].DefaultValue = shipto.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmItemName"].DefaultValue = itemname.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmFgItem"].DefaultValue = fgitem.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty"].DefaultValue = estqty.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmStyle"].DefaultValue = style.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmPaper1"].DefaultValue = flute.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmPaper2"].DefaultValue = test.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmBoard"].DefaultValue = board.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmCalliper"].DefaultValue = HiddenFieldCaliper.Value;
                CorrugatedEstimateDataSource.SelectParameters["prmCategory"].DefaultValue = categ.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmLength"].DefaultValue = length.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmWidth"].DefaultValue = width.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmDepth"].DefaultValue = depth.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmTab"].DefaultValue = HiddenField2.Value;
                CorrugatedEstimateDataSource.SelectParameters["prmColor"].DefaultValue = color.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmPasses"].DefaultValue = pass.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmCoating"].DefaultValue = coating.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmCoatPasses"].DefaultValue = coatpass.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmQtySet"].DefaultValue = qtyset.Text.Trim();

                //CorrugatedEstimateDataSource.SelectParameters["prmInkFrom"].DefaultValue = inkform.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmPassesFrom"].DefaultValue = passform.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmCoatingFrom"].DefaultValue = coatingform.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmCoatPassesFrom"].DefaultValue = coatpassform.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmPurchManuf"].DefaultValue = HiddenField3.Value;
                CorrugatedEstimateDataSource.SelectParameters["prmEstDate"].DefaultValue = estdate.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmDiein"].DefaultValue = diein.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty2"].DefaultValue = qty2.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty3"].DefaultValue = qty3.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty4"].DefaultValue = qty4.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty5"].DefaultValue = qty5.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty6"].DefaultValue = qty6.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty7"].DefaultValue = qty7.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty8"].DefaultValue = qty8.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty9"].DefaultValue = qty9.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty10"].DefaultValue = qty10.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty11"].DefaultValue = qty11.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty12"].DefaultValue = qty12.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty13"].DefaultValue = qty13.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty14"].DefaultValue = qty14.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty15"].DefaultValue = qty15.Text.Trim();

                CorrugatedEstimateDataSource.SelectParameters["prmRelQty1"].DefaultValue = relqty1.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty2"].DefaultValue = relqty2.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty3"].DefaultValue = relqty3.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty4"].DefaultValue = relqty4.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty5"].DefaultValue = relqty5.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty6"].DefaultValue = relqty6.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty7"].DefaultValue = relqty7.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty8"].DefaultValue = relqty8.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty9"].DefaultValue = relqty9.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty10"].DefaultValue = relqty10.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty11"].DefaultValue = relqty11.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty12"].DefaultValue = relqty12.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty13"].DefaultValue = relqty13.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty14"].DefaultValue = relqty14.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty15"].DefaultValue = relqty15.Text.Trim();
                cheksave = "";
                Session["visible2"] = null;
                GridView1.DataBind();
                FormView_CorrugatedEstimate.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='folding_estimate.aspx'</script>");
            }
        }
    }
    protected void set_button_change_click(object sender, EventArgs e)
    {
        CorrugatedEstimateDataSource.SelectParameters["prmAction"].DefaultValue = "ChangeSet";
    }
    protected void combo_button_change_click(object sender, EventArgs e)
    {
        CorrugatedEstimateDataSource.SelectParameters["prmAction"].DefaultValue = "ChangeTandem";
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {        
        Session["order_folding_formno"]=GridView1.SelectedRow.Cells[17].Text;
        Session["order_folding_blankno"] = GridView1.SelectedRow.Cells[18].Text;
        Session["fold_grid_index"] = GridView1.SelectedIndex;
    }

    protected void Formview_corrugated_unload(object sender, EventArgs e)
    {
        try
        {
            Label est = (Label)FormView_CorrugatedEstimate.FindControl("vEstLabel");
            Label fgitem = (Label)FormView_CorrugatedEstimate.FindControl("vFgItemLabel");
            Label reckey = (Label)FormView_CorrugatedEstimate.FindControl("Label_reckey");
            //Label efbrws = (Label)FormView_CorrugatedEstimate.FindControl("Label_efbrws");            

            Session["order_folding_est"] = est.Text.Trim();
            Session["order_rec_key"] = reckey.Text.Trim();
            //Session["order_efbrows"] = efbrws.Text.Trim();
            Session["order_entry_est_no"] = est.Text.Trim();
            Session["item"] = fgitem.Text;
        }
        catch { }
    }

    protected void BoardTextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox board   = (TextBox)FormView_CorrugatedEstimate.FindControl("vBoardTextBox");
        TextBox style   = (TextBox)FormView_CorrugatedEstimate.FindControl("vStyleTextBox");
        TextBox caliper = (TextBox)FormView_CorrugatedEstimate.FindControl("vCaliperTextBox");
        TextBox category = (TextBox)FormView_CorrugatedEstimate.FindControl("vCategoryTextBox");
        TextBox paper1 = (TextBox)FormView_CorrugatedEstimate.FindControl("vFluteTextBox");
        

        try
        {
            if (board.Text != "")
            {
                if (style.Text != "")
                {
                    DataSet ds = new DataSet();
                    Corrugated corr = new Corrugated();
                    ds = corr.SelectBoard("search", UserLogin.UserName, "i-no", "EQUAL", board.Text, "", "", style.Text);

                    caliper.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);

                    Page.ClientScript.RegisterStartupScript
                                (this.GetType(), "showsavebutton", "showsavebutton();", true);
                    if (paper1.Visible == false)
                    {
                        category.Focus();
                    }
                    else
                    {
                        paper1.Focus();
                    }
                }
            }
            else
            {
                board.Focus();
                
            }
                     
        }
        catch {
            HttpContext.Current.Response.Write("<script>alert('Invalid Board!')</script>");
            cheksave = "Yes";
            board.Text = "";
            caliper.Text = "";
            board.Focus();                        

            Page.ClientScript.RegisterStartupScript
                        (this.GetType(), "showsavebutton", "showsavebutton();", true);
        }
                
    }


    protected void CategoryTextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox board = (TextBox)FormView_CorrugatedEstimate.FindControl("vBoardTextBox");
        TextBox style = (TextBox)FormView_CorrugatedEstimate.FindControl("vStyleTextBox");
        TextBox caliper = (TextBox)FormView_CorrugatedEstimate.FindControl("vCaliperTextBox");
        TextBox category = (TextBox)FormView_CorrugatedEstimate.FindControl("vCategoryTextBox");
        TextBox paper1 = (TextBox)FormView_CorrugatedEstimate.FindControl("vFluteTextBox");
        TextBox length = (TextBox)FormView_CorrugatedEstimate.FindControl("vLenghtTextBox");

        try
        {
            if (category.Text != "")
            {
                LookUp lookup = new LookUp();
                DataSet ds = new DataSet();

                ds = lookup.CategoryLook(UserLogin.UserName, "search", "procat", "EQUAL", category.Text);

                if (ds.Tables[0].Rows.Count == 0)
                {
                    HttpContext.Current.Response.Write("<script>alert('Invalid Category!')</script>");
                    cheksave = "Yes";
                    category.Text = "";
                    category.Focus();
                }
                else
                {
                    length.Focus();                    
                }
            }
            else
            {
                category.Focus();
            }
        }
        catch
        {

        }
    }

    protected void StyleTextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox board = (TextBox)FormView_CorrugatedEstimate.FindControl("vBoardTextBox");
        TextBox style = (TextBox)FormView_CorrugatedEstimate.FindControl("vStyleTextBox");
        
        try
        {
            if (style.Text != "")
            {
                DataSet ds = new DataSet();
                Corrugated corr = new Corrugated();
                ds = corr.Selectstyle("search", UserLogin.UserName, "EQUAL", style.Text, "1");

                if (ds.Tables[0].Rows.Count == 0)
                {
                    HttpContext.Current.Response.Write("<script>alert('Invalid Style!')</script>");
                    cheksave = "Yes";
                    style.Text = "";
                    style.Focus();
                }
                else
                {
                    board.Focus();
                }
            }
            else
            {
               style.Focus();
            }
        }
        catch
        {

        } 
    }

    protected void estdttextchanged(object sender, EventArgs e)
    { 
        
    }
    protected void blank_est_Click(object sender, EventArgs e)
    {
        Session["visible2"] = 3;
        CorrugatedEstimateDataSource.SelectParameters["prmUser"].DefaultValue = "Admin" ;
        CorrugatedEstimateDataSource.SelectParameters["prmAction"].DefaultValue = "viewblank";

    }

    protected void blank_save_Click(object sender, EventArgs e)
    {
        if (cheksave != "Yes")
        {
            Label est = (Label)FormView_CorrugatedEstimate.FindControl("vEstLabel");
            TextBox cust = (TextBox)FormView_CorrugatedEstimate.FindControl("vCustTextBox");
            TextBox fgitem = (TextBox)FormView_CorrugatedEstimate.FindControl("vFgItemTextBox");
            TextBox custpart = (TextBox)FormView_CorrugatedEstimate.FindControl("vCustPartTextBox");
            TextBox shipto = (TextBox)FormView_CorrugatedEstimate.FindControl("vShipToTextBox");
            TextBox itemname = (TextBox)FormView_CorrugatedEstimate.FindControl("vItemNameTextBox");
            TextBox estqty = (TextBox)FormView_CorrugatedEstimate.FindControl("vEstQtyTextBox");
            TextBox style = (TextBox)FormView_CorrugatedEstimate.FindControl("vStyleTextBox");
            TextBox flute = (TextBox)FormView_CorrugatedEstimate.FindControl("vFluteTextBox");
            TextBox test = (TextBox)FormView_CorrugatedEstimate.FindControl("vTestTextBox");
            TextBox board = (TextBox)FormView_CorrugatedEstimate.FindControl("vBoardTextBox");

            TextBox caliper = (TextBox)FormView_CorrugatedEstimate.FindControl("vCaliperTextBox");
            TextBox categ = (TextBox)FormView_CorrugatedEstimate.FindControl("vCategoryTextBox");
            TextBox length = (TextBox)FormView_CorrugatedEstimate.FindControl("vLenghtTextBox");
            TextBox width = (TextBox)FormView_CorrugatedEstimate.FindControl("vWidthTextBox");
            TextBox depth = (TextBox)FormView_CorrugatedEstimate.FindControl("vDepthTextBox");
            TextBox color = (TextBox)FormView_CorrugatedEstimate.FindControl("vColorTextBox");
            //TextBox pass = (TextBox)FormView_CorrugatedEstimate.FindControl("vPassesTextBox");

            TextBox coating = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatingTextBox");
            //TextBox coatpass = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatPassesTextBox");
            TextBox qtyset = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtySetTextBox");
            //TextBox inkform = (TextBox)FormView_CorrugatedEstimate.FindControl("vInkFromTextBox");
            //TextBox passform = (TextBox)FormView_CorrugatedEstimate.FindControl("vPassesFromTextBox");
            //TextBox coatingform = (TextBox)FormView_CorrugatedEstimate.FindControl("vCoatingFromTextBox");
            TextBox diein = (TextBox)FormView_CorrugatedEstimate.FindControl("vDieInTextBox");
            DropDownList purch = (DropDownList)FormView_CorrugatedEstimate.FindControl("DropDownList2");
            TextBox estdate = (TextBox)FormView_CorrugatedEstimate.FindControl("vEstDateTextBox");

            TextBox qty1 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent1TextBox");
            TextBox qty2 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent2TextBox");
            TextBox qty3 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent3TextBox");
            TextBox qty4 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent4TextBox");
            TextBox qty5 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent5TextBox");
            TextBox qty6 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent6TextBox");
            TextBox qty7 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent7TextBox");
            TextBox qty8 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent8TextBox");
            TextBox qty9 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent9TextBox");
            TextBox qty10 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent10TextBox");
            TextBox qty11 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent11TextBox");
            TextBox qty12 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent12TextBox");
            TextBox qty13 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent13TextBox");
            TextBox qty14 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent14TextBox");
            TextBox qty15 = (TextBox)FormView_CorrugatedEstimate.FindControl("vQtyExtent15TextBox");


            TextBox relqty1 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent1TextBox");
            TextBox relqty2 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent2TextBox");
            TextBox relqty3 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent3TextBox");
            TextBox relqty4 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent4TextBox");
            TextBox relqty5 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent5TextBox");
            TextBox relqty6 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent6TextBox");
            TextBox relqty7 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent7TextBox");
            TextBox relqty8 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent8TextBox");
            TextBox relqty9 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent9TextBox");
            TextBox relqty10 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent10TextBox");
            TextBox relqty11 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent11TextBox");
            TextBox relqty12 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent12TextBox");
            TextBox relqty13 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent13TextBox");
            TextBox relqty14 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent14TextBox");
            TextBox relqty15 = (TextBox)FormView_CorrugatedEstimate.FindControl("vRelQtyExtent15TextBox");

            if (purch.SelectedIndex == 0)
            {
                HiddenField3.Value = "M";
            }
            if (purch.SelectedIndex == 1)
            {
                HiddenField3.Value = "P";
            }
            if (length.Text == "")
                length.Text = "0";
            if (width.Text == "")
                width.Text = "0";
            if (depth.Text == "")
                depth.Text = "0";
            if (color.Text == "")
                color.Text = "0";
            if (coating.Text == "")
                coating.Text = "0";
            if (diein.Text == "")
                diein.Text = "0";
            if (qtyset.Text == "")
                qtyset.Text = "0";

            if (HiddenFieldCaliper.Value == "" || HiddenFieldCaliper.Value == null)
                HiddenFieldCaliper.Value = "0";

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated corr = new Corrugated();

            bool check = corr.ValidateFodingEst("Update", UserLogin.UserName, Convert.ToString(Session["order_folding_est"]), cust.Text.Trim(), custpart.Text.Trim(), shipto.Text.Trim(), itemname.Text.Trim(), fgitem.Text.Trim(), Convert.ToDecimal(estqty.Text.Trim()), style.Text.Trim(), flute.Text.Trim(), test.Text.Trim(), board.Text.Trim(), 0, categ.Text.Trim(), Convert.ToDecimal(length.Text.Trim()), Convert.ToDecimal(width.Text.Trim()), Convert.ToDecimal(depth.Text.Trim()), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(Session["order_folding_blankno"]), "", Convert.ToInt32(color.Text.Trim()), 0, Convert.ToInt32(coating.Text.Trim()), 0, Convert.ToDecimal(qtyset.Text.Trim()), 0, 0, 0, 0, purch.Text.Trim(), Convert.ToDateTime("11/11/2009"), HiddenField1.Value, 0);

            string value = Convert.ToString(check);
            if (value == "True")
            {
                CorrugatedEstimateDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                CorrugatedEstimateDataSource.SelectParameters["prmAction"].DefaultValue = "BlankSave";
                CorrugatedEstimateDataSource.SelectParameters["prmCust"].DefaultValue = cust.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmCustPart"].DefaultValue = custpart.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmShipTo"].DefaultValue = shipto.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmItemName"].DefaultValue = itemname.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmFgItem"].DefaultValue = fgitem.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty"].DefaultValue = estqty.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmStyle"].DefaultValue = style.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmPaper1"].DefaultValue = flute.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmPaper2"].DefaultValue = test.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmBoard"].DefaultValue = board.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmCalliper"].DefaultValue = HiddenFieldCaliper.Value;
                CorrugatedEstimateDataSource.SelectParameters["prmCategory"].DefaultValue = categ.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmLength"].DefaultValue = length.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmWidth"].DefaultValue = width.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmDepth"].DefaultValue = depth.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmTab"].DefaultValue = HiddenField2.Value;
                CorrugatedEstimateDataSource.SelectParameters["prmColor"].DefaultValue = color.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmPasses"].DefaultValue = pass.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmCoating"].DefaultValue = coating.Text.Trim();
                //CorrugatedEstimateDataSource.SelectParameters["prmCoatPasses"].DefaultValue = coatpass.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmQtySet"].DefaultValue = qtyset.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmPurchManuf"].DefaultValue = HiddenField3.Value;
                CorrugatedEstimateDataSource.SelectParameters["prmEstDate"].DefaultValue = estdate.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmDiein"].DefaultValue = diein.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty2"].DefaultValue = qty2.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty3"].DefaultValue = qty3.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty4"].DefaultValue = qty4.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty5"].DefaultValue = qty5.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty6"].DefaultValue = qty6.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty7"].DefaultValue = qty7.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty8"].DefaultValue = qty8.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty9"].DefaultValue = qty9.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty10"].DefaultValue = qty10.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty11"].DefaultValue = qty11.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty12"].DefaultValue = qty12.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty13"].DefaultValue = qty13.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty14"].DefaultValue = qty14.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmEstQty15"].DefaultValue = qty15.Text.Trim();

                CorrugatedEstimateDataSource.SelectParameters["prmRelQty1"].DefaultValue = relqty1.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty2"].DefaultValue = relqty2.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty3"].DefaultValue = relqty3.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty4"].DefaultValue = relqty4.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty5"].DefaultValue = relqty5.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty6"].DefaultValue = relqty6.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty7"].DefaultValue = relqty7.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty8"].DefaultValue = relqty8.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty9"].DefaultValue = relqty9.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty10"].DefaultValue = relqty10.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty11"].DefaultValue = relqty11.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty12"].DefaultValue = relqty12.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty13"].DefaultValue = relqty13.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty14"].DefaultValue = relqty14.Text.Trim();
                CorrugatedEstimateDataSource.SelectParameters["prmRelQty15"].DefaultValue = relqty15.Text.Trim();
                GridView1.DataBind();
                cheksave = "";
                FormView_CorrugatedEstimate.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='folding_estimate.aspx'</script>");
            }
        }
    }
}

